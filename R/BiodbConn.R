#' The mother abstract class of all database connectors.
#'
#' This is the super class of all connector classes. All methods defined here
#' are thus common to all connector classes. All connector classes inherit
#' from this abstract class.
#'
#' See section Fields for a list of the constructor's
#' parameters. Concrete classes may have direct web services methods or other
#' specific methods implemented, in which case they will be described inside the
#' documentation of the concrete class. Please refer to the documentation of
#' each concrete class for more information. The database direct web services
#' methods will be named "ws.*".
#'
#' The constructor has the following arguments:
#'
#' id: The identifier of the connector.
#'
#' cache.id: The identifier used in the disk cache.
#'
#' @seealso Super class \code{\link{BiodbConnBase}}, and
#' \code{\link{BiodbFactory}} class.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::newInst()
#'
#' # Get a compound CSV file database
#' chebi.tsv <- system.file("extdata", "chebi_extract.tsv", package='biodb')
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('comp.csv.file', url=chebi.tsv)
#'
#' # Get 10 identifiers from the database:
#' ids <- conn$getEntryIds(10)
#'
#' # Get number of entries contained in the database:
#' n <- conn$getNbEntries()
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @import R6
#' @import openssl
#' @import plyr
#' @include BiodbConnBase.R
#' @export
BiodbConn <- R6::R6Class("BiodbConn",
inherit=BiodbConnBase,

public=list(

#' @description
#' New instance initializer. Connector objects must not be created directly.
#' Instead, you create new connector instances through the BiodbFactory
#' instance.
#' @param id The ID of the connector instance.
#' @param cache.id The Cache ID of the connector instance.
#' @param bdb The BiodbMain instance.
#' @param ... Remaining arguments will be passed to the constructor of the
#' super class.
#' @return Nothing.
initialize=function(id=NA_character_, cache.id=NA_character_, bdb, ...) {

    super$initialize(...)
    abstractClass('BiodbConn', self)

    logDebug("Initialize connector %s.", id)
    chk::chk_character(id)
    chk::chk_is(bdb, 'BiodbMain')
    private$bdb <- bdb
    private$id <- id
    private$cache.id <- if (is.null(cache.id)) NA_character_ else cache.id
    private$entries <- list()

    # Register with request scheduler
    if (self$isRemotedb()) {
        logDebug("Register connector %s with the request scheduler", id)
        private$bdb$getRequestScheduler()$registerConnector(self)
    }

    return(invisible(NULL))
},

#' @description
#' Returns the biodb main class instance to which this object is
#' attached.
#' @return The main biodb instance.
getBiodb=function() {

    return(private$bdb)
},

#' @description
#' Get the identifier of this connector.
#' @return The identifier of this connector.
getId=function() {

    return(private$id)
},

#' @description
#' Prints a description of this connector.
#' @return Nothing.
print=function() {
    
    super$print() 
    cat("  ID: ", private$id, ".\n", sep='')

    return(invisible(NULL))
},

#' @description
#' Correct a vector of IDs by formatting them to the database official
#' format, if required and possible.
#' @param ids A character vector of IDs.
#' @return The vector of IDs corrected.
correctIds=function(ids) {
    return(private$doCorrectIds(ids))
},

#' @description
#' Return the entry corresponding to this ID. You can pass a vector of
#' IDs, and you will get a list of entries.
#' @param id A character vector containing entry identifiers.
#' @param drop If set to TRUE and only one entry is requrested, then the
#' returned value will be a single BiodbEntry object, otherwise it will be
#' a list of BiodbEntry objects.
#' @param nulls If set to TRUE, NULL entries are preserved. This ensures
#' that the output list has the same length than the input vector `id`.
#' Otherwise they are removed from the final list.
#' @return A list of BiodbEntry objects, the same size of the vector of
#' IDs. The list will contain NULL values for invalid IDs. If drop is set
#' to TRUE and only one etrny was requested then a single BiodbEntry is
#' returned instead of a list.
getEntry=function(id, drop=TRUE, nulls=TRUE) {

    entries <- private$bdb$getFactory()$getEntry(self$getId(), id=id,
        drop=drop)

    if ( ! nulls && is.list(entries))
        entries <- Filter(function(e) ! is.null(e), entries)

    return(entries)
},

#' @description
#' Get the path to the persistent cache file.
#' @param entry.id The identifiers (e.g.: accession numbers) as a
#' character vector of the database entries.
#' @return A character vector, the same length as the vector of IDs,
#' containing the paths to the cache files corresponding to the requested
#' entry IDs.
getCacheFile=function(entry.id) {

    c <- private$bdb$getPersistentCache()
    fp <- c$getFilePath(self$getCacheId(), entry.id, self$getEntryFileExt())

    return(fp)
},

#' @description
#' Get the contents of database entries from IDs (accession numbers).
#' @param id A character vector of entry IDs.
#' @return A character vector containing the contents of the requested
#' IDs. If no content is available for an entry ID, then NA will be used.
getEntryContent=function(id) {

    content <- list()
    cch <- private$bdb$getPersistentCache()
    nm <- self$getPropertyValue('name')

    if ( ! is.null(id) && length(id) > 0) {

        id <- as.character(id)

        # Debug
        logDebug0("Get ", nm, " entry content(s) for ", length(id),
            " id(s)...")

        # Download full database
        if (self$isDownloadable())
            self$download()

        # Initialize content
        if (cch$isReadable(self) && ! is.null(self$getCacheId())) {
            # Load content from cache
            content <- cch$loadFileContent(self$getCacheId(),
                name=id, ext=self$getEntryFileExt())
            missing.ids <- id[vapply(content, is.null, FUN.VALUE=TRUE)]
        }
        else {
            content <- lapply(id, as.null)
            missing.ids <- id
        }

        # Remove duplicates
        n.duplicates <- sum(duplicated(missing.ids))
        missing.ids <- missing.ids[ ! duplicated(missing.ids)]
        
        # Remove NAs
        missing.ids <- missing.ids[ ! is.na(missing.ids)]

        # Debug
        if (any(is.na(id)))
            logDebug("%d %s entry ids are NA.", sum(is.na(id)), nm)
        if (cch$isReadable(self)) {
            nld <- sum( ! is.na(id)) - length(missing.ids)
            logDebug("%d %s entry content(s) loaded from cache.", nld, nm)
            if (n.duplicates > 0)
                logDebug0(n.duplicates, " ", nm, " entry ids, whose content",
                    " needs to be fetched, are duplicates.")
        }

        # Get contents
        if (length(missing.ids) > 0
            && ( ! self$isDownloadable() || ! self$isDownloaded())) {

            logDebug0(length(missing.ids), " entry content(s) need to be ",
                "fetched from ", nm, " database \"",
                self$getPropValSlot('urls', 'base.url'), "\".")

            # Divide list of missing ids in chunks
            # (in order to save in cache regularly)
            cs <- private$bdb$getConfig()$get('dwnld.chunk.size')
            logDebug('dwnld.chunk.size=%d', cs)
            chunks.of.missing.ids <- if (is.na(cs)) list(missing.ids)
                else split(missing.ids, ceiling(seq_along(missing.ids) / cs))
            logDebug('%d chunk(s) to download.', length(chunks.of.missing.ids))

            # Loop on chunks
            missing.contents <- NULL
            for (ch.missing.ids in chunks.of.missing.ids) {

                # Get contents of missing entries
                ec <- self$getEntryContentFromDb(ch.missing.ids)

                # Save to cache
                if ( ! is.null(ec)
                    && ! is.null(self$getCacheId()) && cch$isWritable(self))
                    cch$saveContentToFile(ec,
                        cache.id=self$getCacheId(), name=ch.missing.ids,
                        ext=self$getEntryFileExt())

                # Append
                missing.contents <- c(missing.contents, ec)

                # Debug
                if (cch$isReadable(self)) {
                    n <- length(missing.ids) - length(missing.contents)
                    logDebug("Now %d id(s) left to be retrieved...", n)
                }
            }

            # Merge content and missing.contents
            missing.contents <- as.list(missing.contents)
            ii <- vapply(id[id %in% missing.ids],
                function(x) which(missing.ids == x), FUN.VALUE=1L)
            content[id %in% missing.ids] <- missing.contents[ii]
        }
    }

    return(content)
},

#' @description
#' Get the contents of entries directly from the database. A direct
#' request or an access to the database will be made in order to retrieve the
#' contents. No access to the biodb cache system will be made.
#' @param entry.id A character vector with the IDs of entries to retrieve.
#' @return A character vector, the same size of entry.id, with
#' contents of the requested entries. An NA value will be set for the content
#' of each entry for which the retrieval failed.
getEntryContentFromDb=function(entry.id) {
    return(private$doGetEntryContentFromDb(entry.id))
},

#' @description
#' Gets the URL to use in order to get the contents of the specified
#'     entries.
#' @param entry.id A character vector with the IDs of entries to retrieve.
#' @param concatenate If set to TRUE, then try to build as few URLs as
#' possible, sending requests with several identifiers at once.
#' @param max.length The maximum length of the URLs to return, in
#' number of characters.
#' @return A vector of URL strings.
getEntryContentRequest=function(entry.id, concatenate=TRUE, max.length=0) {

    private$checkIsRemote()
    urls <- character(0)

    if (length(entry.id) > 0) {

        # Get full URL
        full.url <- private$doGetEntryContentRequest(entry.id,
            concatenate=concatenate)

        # Are URLs acceptable?
        if (max.length == 0 # OK, no restrictions.
            || all(nchar(full.url) <= max.length) # OK, no URL is too long
            || length(entry.id) <= length(full.url)) # OK, fewer IDs than URLs
            urls <- full.url

        # Some URLs are too long and we have more IDs than URLs,
        # so we try to create more URLs with less IDs in each.
        else {
            logDebug("Split full URL.")

            start <- 1

            # Loop as long as there are IDs
            while (start <= length(entry.id)) {
                # Find max size URL
                a <- start
                b <- length(entry.id)
                while (a < b) {
                    m <- as.integer((a + b) / 2)
                    url <- private$doGetEntryContentRequest(entry.id[start:m])
                    if (all(nchar(url) <= max.length) && m != a)
                        a <- m
                    else
                        b <- m
                }
                urls <- c(urls,
                    private$doGetEntryContentRequest(entry.id[start:a]))
                start <- a + 1
            }
        }
    }

    return(urls)
},

#' @description
#' Get entry identifiers from the database. More arguments can be given,
#' depending on implementation in specific databases. For mass databases the
#' ms.level argument can also be set.
#' @param max.results The maximum of elements to return from the method.
#' @param ... Arguments specific to connectors.
#' @return A character vector containing entry IDs from the database. An empty
#' vector for a remote database may mean that the database does not support
#' requesting for entry accessions.
getEntryIds=function(max.results=0, ...) {

    chk::chk_number(max.results)
    chk::chk_gte(max.results, 0)

    ids <- character()

    # Get IDs from volatile cache
    not.null <- ! vapply(private$entries, is.null, FUN.VALUE=TRUE)
    ids <- names(private$entries[not.null])

    # Get IDs from database
    if (max.results == 0 || length(ids) < max.results) {
        db.ids <- private$doGetEntryIds(max.results=max.results, ...)
        if ( ! is.null(db.ids)) {
            db.ids <- as.character(db.ids)
            ids <- c(ids, db.ids[ ! db.ids %in% ids])
        }
    }

    # Cut
    if (max.results > 0 && length(ids) > max.results)
        ids <- ids[seq_len(max.results)]

    return(ids)
},

#' @description
#' Get the number of entries contained in this database.
#' @param count If set to TRUE and no straightforward way exists to get number
#' of entries, count the output of getEntryIds().
#' @return The number of entries in the database, as an integer.
getNbEntries=function(count=FALSE) {
    return(private$doGetNbEntries(count=count))
},

#' @description
#' Tests if this connector is able to edit the database (i.e.: the connector
#' class implements the interface BiodbEditable). If this connector is
#' editable, then you can call allowEditing() to enable editing.
#' @return Returns TRUE if the database is editable.
isEditable=function() {

    return(self$getPropertyValue('editable'))
},

#' @description
#' Tests if editing is allowed.
#' @return TRUE if editing is allowed for this database, FALSE
#'     otherwise.
editingIsAllowed=function() {
    
    private$checkIsEditable()
    private$initEditable()

    return(private$editing.allowed)
},

#' @description
#' Allows editing for this database.
#' @return Nothing.
allowEditing=function() {

    private$checkIsEditable()
    self$setEditingAllowed(TRUE)

    return(invisible(NULL))
},

#' @description
#' Disallows editing for this database.
#' @return Nothing.
disallowEditing=function() {
    
    private$checkIsEditable()
    self$setEditingAllowed(FALSE)

    return(invisible(NULL))
},

#' @description
#' Allow or disallow editing for this database.
#' @param allow A logical value.
#' @return Nothing.
setEditingAllowed=function(allow) {
    
    chk::chk_logical(allow)

    private$checkIsEditable()
    private$editing.allowed <- allow

    return(invisible(NULL))
},

#' @description
#' Adds a new entry to the database. The passed entry must have been previously
#' created from scratch using BiodbFactory :createNewEntry() or cloned from an
#' existing entry using BiodbEntry :clone().
#' @param entry The new entry to add. It must be a valid BiodbEntry object.
#' @return Nothing.
addNewEntry=function(entry) {

    private$checkIsEditable()
    private$checkEditingIsAllowed()

    # Is already part of a connector instance?
    if (entry$parentIsAConnector())
        error0('Impossible to add entry as a new entry. The passed',
            ' entry is already part of a connector.')

    # No accession number?
    if ( ! entry$hasField('accession'))
        error0('Impossible to add entry as a new entry. The passed entry',
            ' has no accession number.')
    id <- entry$getFieldValue('accession')
    if (is.na(id))
        error0('Impossible to add entry as a new entry. The passed',
            ' entry has an accession number set to NA.')

    # Accession number is already used?
    e <- self$getEntry(id)
    if ( ! is.null(e))
        error0('Impossible to add entry as a new entry. The accession',
            ' number of the passed entry is already used in the',
            ' connector.')

    # Make sure ID field is equal to accession
    id.field <- self$getEntryIdField()
    if ( ! entry$hasField(id.field) || entry$getFieldValue(id.field) != id)
        entry$setFieldValue(id.field, id)

    # Remove entry from non-volatile cache
    cch <- private$bdb$getPersistentCache()
    if (cch$isWritable(self))
        cch$deleteFile(self$getCacheId(), name=id, ext=self$getEntryFileExt())

    # Flag entry as new
    entry$.__enclos_env__$private$setAsNew(TRUE)

    # Set the connector as its parent
    entry$.__enclos_env__$private$setParent(self)

    # Add entry to volatile cache
    private$addEntriesToCache(id, list(entry))

    return(invisible(NULL))
},

#' @description
#' Tests if this connector is able to write into the database.  If this
#' connector is writable, then you can call allowWriting() to enable writing.
#' @return Returns TRUE if the database is writable.
isWritable=function() {

    return(self$getPropertyValue('writable'))
},

#' @description
#' Allows the connector to write into this database.
#' @return Nothing.
allowWriting=function() {

    private$checkIsWritable()
    self$setWritingAllowed(TRUE)

    return(invisible(NULL))
},

#' @description
#' Disallows the connector to write into this database.
#' @return Nothing.
disallowWriting=function() {
    
    private$checkIsWritable()
    self$setWritingAllowed(FALSE)

    return(invisible(NULL))
},

#' @description
#' Allows or disallows writing for this database.
#' @param allow If set to TRUE, allows writing.
#' @return Nothing.
setWritingAllowed=function(allow) {
    
    private$checkIsWritable()
    chk::chk_logical(allow)
    private$writing.allowed <- allow

    return(invisible(NULL))
},

#' @description
#' Tests if the connector has access right to the database.
#' @return TRUE if writing is allowed for this database, FALSE
#'     otherwise.
writingIsAllowed=function() {
    
    private$checkIsWritable()
    private$initWritable()

    return(private$writing.allowed)
},

#' @description
#' Writes into the database. All modifications made to the database since
#'     the last time write() was called will be saved.
#' @return Nothing.
write=function() {

    private$checkIsWritable()
    private$checkWritingIsAllowed()
    private$doWrite()

    # Unset "new" flag for all entries
    for (e in private$entries)
        e$.__enclos_env__$private$setAsNew(FALSE)

    return(invisible(NULL))
},

#' @description
#' Tests if a field can be used to search entries when using method
#' searchForEntries().
#' @param field The name of the field.
#' @param field.type The field type.
#' @return Returns TRUE if the database is searchable using the specified
#' field or searchable by any field of the specified type, FALSE otherwise.
isSearchableByField=function(field=NULL, field.type=NULL) {

    if (is.null(field) && is.null(field.type))
        error("Either field or field.type must be set. Both are NULL.")
    chk::chk_null_or(field, vld=chk::vld_string)
    chk::chk_null_or(field.type, vld=chk::vld_string)
    field.type <- tolower(field.type)

    v <- FALSE
    ef <- private$bdb$getEntryFields()

    # Check if a field is searchable
    if ( ! is.null(field)) {
        field <- ef$getRealName(field)
        for (sf in self$getPropertyValue('searchable.fields'))
            if (ef$getRealName(sf) == field) {
                v <- private$doHasField(sf)
                break
            }
    }
    
    # Check if one of the searchable field is of the required type
    else {
        for (sf in self$getPropertyValue('searchable.fields'))
            if (ef$get(sf)$isOfType(field.type)) {
                v <- private$doHasField(sf)
                break
            }
    }

    return(v)
},

#' @description
#' Get the list of all searchable fields.
#' @return A character vector containing all searchable fields for this
#' connector.
getSearchableFields=function() {
    
    # Get all searchable candidates
    fields <- self$getPropertyValue('searchable.fields')
    
    # Filter out those that are not searchable with this instance
    # This is for dynamic connectors like CsvFileConn whose list of available
    # fields may vary.
    fields <- Filter(function(f) self$isSearchableByField(f), fields)
    
    return(fields)
},

#' @description
#' Searches the database for entries whose name matches the specified name.
#' Returns a character vector of entry IDs.
#' @param fields A list of fields on which to filter entries. To get a match,
#' all fields must be matched (i.e. logical AND). The keys of the list are the
#' entry field names on which to filter, and the values are the filtering
#' parameters. For character fields, the filter parameter is a character vector
#' in which all strings must be found inside the field's value. For numeric
#' fields, the filter parameter is either a list specifying a min-max range
#' (`list(min=1.0, max=2.5)`) or a value with a tolerance in delta
#' (`list(value=2.0, delta=0.1)`) or ppm (`list(value=2.0, ppm=1.0)`).
#' @param max.results If set, the number of returned IDs is limited to this
#' number.
#' @return A character vector of entry IDs whose name matches the requested
#' name.
searchForEntries=function(fields=NULL, max.results=0) {

    chk::chk_null_or(fields, vld=chk::vld_list)
    chk::chk_number(max.results)
    chk::chk_gte(max.results, 0)

    ids <- NULL
    wrong_fields <- FALSE
    if (is.null(fields))
        fields <- list()

    # Check if field can be used for searching
    for (f in names(fields)) {
        
        # Remove field if NULL
        if (is.null(fields[[f]])) {
            fields[[f]] <- NULL
            wrong_fields <- TRUE
        }
    
        # Error if field is not searchable
        else if ( ! self$isSearchableByField(f)) {
            warn('This database is not searchable by field "%s".', f)
            fields[[f]] <- NULL
            wrong_fields <- TRUE
        }
    }
        
    # Call concrete method
    if (length(fields) > 0 || ! wrong_fields)
        ids <- private$doSearchForEntries(fields=fields,
            max.results=max.results)

    # Convert NULL to empty list
    if (is.null(ids))
        ids <- character()

    # Cut
    if (max.results > 0 && max.results < length(ids))
        ids <- ids[seq_len(max.results)]

    return(ids)
},

#' @description
#' DEPRECATED. Use searchForEntries() instead.
#' @param name A character value to search inside name fields.
#' @param max.results If set, the number of returned IDs is limited to this
#' number.
#' @return A character vector of entry IDs whose name matches the
#' requested name.
searchByName=function(name, max.results=0) {
    
    lifecycle::deprecate_warn('1.0.0', 'searchByName()', "searchForEntries()")

    ids <- self$searchForEntries(list(name=name), max.results=max.results)

    return(ids)
},

#' @description
#' Tests if the connector can download the database.
#' @return Returns TRUE if the database is downloadable.
isDownloadable=function() {

    return(self$getPropertyValue('downloadable'))
},

#' @description
#' Tests if the database has been downloaded.
#' @return TRUE if the database content has already been downloaded.
isDownloaded=function() {

    private$checkIsDownloadable()
    cch <- private$bdb$getPersistentCache()
    dwnlded  <- cch$markerExists(self$getCacheId(), name='downloaded')

    s <- (if (dwnlded) 'already' else 'not yet')
    logDebug0('Database ', self$getId(), ' has ', s, ' been downloaded.')

    return(dwnlded)
},

#' @description
#' Tests if the connector requires the download of the database.
#' @return TRUE if the connector requires download of the database.
requiresDownload=function() {
    private$checkIsDownloadable()
    return(private$doesRequireDownload())
},

#' @description
#' Gets the path where the downloaded content is written.
#' @return The path where the downloaded database is written.
getDownloadPath=function() {

    private$checkIsDownloadable()
    cch <- private$bdb$getPersistentCache()
    ext <- self$getPropertyValue('dwnld.ext')
    path <- cch$getFilePath(self$getCacheId(), name='download', ext=ext)

    logDebug0('Download path of ', self$getId(), ' is "', path, '".')

    return(path)
},

#' @description
#' Set the downloaded file into the cache.
#' @param src Path to the downloaded file.
#' @param action Specifies if files have to be moved or copied into the cache.
#' @return Nothing.
setDownloadedFile=function(src, action=c('copy', 'move')) {

    private$checkIsDownloadable()
    cch <- private$bdb$getPersistentCache()
    ext <- self$getPropertyValue('dwnld.ext')
    name <- 'download'
    cache.id <- self$getCacheId()

    # Remove if already exists
    if (cch$fileExists(cache.id=cache.id, name=name, ext=ext))
        cch$deleteFile(cache.id=cache.id, name=name, ext=ext)

    # Import
    cch$addFilesToCache(src, cache.id=cache.id, name=name, ext=ext,
        action=action)

    return(invisible(NULL))
},

#' @description
#' Tests if the downloaded database has been extracted (in case the
#'     database needs extraction).
#' @return TRUE if the downloaded database content has been
#'     extracted, FALSE otherwise.
isExtracted=function() {

    private$checkIsDownloadable()
    cch <- private$bdb$getPersistentCache()
    return(cch$markerExists(self$getCacheId(), name='extracted'))
},

#' @description
#' Downloads the database content locally.
#' @return Nothing.
download=function() {

    private$checkIsDownloadable()
    cch <- private$bdb$getPersistentCache()

    # Download
    cfg <- private$bdb$getConfig()
    if (cch$isWritable(self) && ! self$isDownloaded()
        && (cfg$isEnabled('allow.huge.downloads') || self$requiresDownload())
        && ! cfg$isEnabled('offline')) {

        logInfo0("Downloading whole database of ", self$getId(), ".")
        private$doDownload()
        if ( ! file.exists(self$getDownloadPath()))
            error("File %s does not exists. Downloading went wrong.",
                self$getDownloadPath())
        logDebug0('Downloading of ', self$getId(), ' completed.')

        # Set marker
        cch$setMarker(self$getCacheId(), name='downloaded')
    }

    # Extract
    if (self$isDownloaded() && ! self$isExtracted()) {

        logInfo0("Extract whole database of ", self$getId(), ".")

        private$doExtractDownload()

        # Set marker
        cch$setMarker(self$getCacheId(), name='extracted')
    }

    return(invisible(NULL))
},

#' @description
#' Tests if the connector is connected to a remote database.
#' @return Returns TRUE if the database is a remote database."
isRemotedb=function() {
    return(self$getPropertyValue('remote'))
},

#' @description
#' Tests if the connector's database is a compound database.
#' @return Returns TRUE if the database is a compound database.
isCompounddb=function() {

    return(self$getPropertyValue('compound.db'))
},

#' @description
#' This method is deprecated. Use searchForEntries() instead.
#' Searches for compounds by name and/or by mass. At least one of name or
#' mass must be set.
#' @param name The name of a compound to search for.
#' @param description A character vector of words or expressions to search for
#' inside description field. The words will be searched in order. A match will
#' be made only if all words are inside the description field.
#' @param mass The searched mass.
#' @param mass.field For searching by mass, you must indicate a mass field to
#' use ('monoisotopic.mass', 'molecular.mass', 'average.mass' or
#' 'nominal.mass').
#' @param mass.tol The tolerance value on the molecular mass.
#' @param mass.tol.unit The type of mass tolerance. Either 'plain' or 'ppm'.
#' @param max.results The maximum number of matches to return.
#' @return A character vector of entry IDs."
searchCompound=function(name=NULL, mass=NULL, mass.field=NULL, mass.tol=0.01,
    mass.tol.unit='plain', max.results=0) {
    lifecycle::deprecate_warn('1.0.0', 'searchCompound()',
        "searchForEntries()")
    private$checkIsCompounddb()
    private$checkMassField(mass=mass, mass.field=mass.field)

    ids <- NULL

    # Try searchForEntries
    if ( ! is.null(name) && is.null(mass))
        ids <- self$searchForEntries(list(name=name), max.results=max.results)
    else if ( ! is.null(mass)) {
        fields <- if (is.null(name)) list() else list(name=name)
        fields[[mass.field]] <- list(value=mass)
        if (mass.tol.unit == 'ppm')
            fields[[mass.field]]$ppm = mass.tol
        else
            fields[[mass.field]]$delta = mass.tol
        ids <- self$searchForEntries(fields, max.results=max.results)
    }

    return(ids)
},

#' @description
#' Annotates a mass spectrum with the database. For each matching entry the
#' entry field values will be set inside columns appended to the data frame.
#' Names of these columns will use a common prefix in order to distinguish them
#' from other data from the input data frame.
#' @param x Either a data frame or a numeric vector containing the M/Z values.
#' @param mz.col The name of the column where to find M/Z values in case x is a
#' data frame.
#' @param ms.mode The MS mode. Set it to either 'neg' or 'pos'.
#' @param mz.tol The tolerance on the M/Z values. 
#' @param mz.tol.unit The type of the M/Z tolerance. Set it to either to 'ppm'
#' or 'plain'.
#' @param mass.field The mass field to use for matching M/Z values. One of:
#' 'monoisotopic.mass', 'molecular.mass', 'average.mass', 'nominal.mass'.
#' @param fields A character vector containing the additional entry fields you
#' would like to get for each matched entry. Each field will be output in a
#' different column.
#' @param insert.input.values Insert input values at the beginning of the
#' result data frame.
#' @param prefix A prefix that will be inserted before the name of each added
#' column in the output. By default it will be set to the name of the database
#' followed by a dot.
#' @param fieldsLimit The maximum of values to output for fields with multiple
#' values. Set it to 0 to get all values.
#' @param max.results If set, it is used to limit the number of matches found
#' for each M/Z value. To get all the matches, set this parameter to
#' NA_integer_.  Default value is 3.
#' @return A data frame containing the input values, and annotation columns
#' appended at the end. The first annotation column contains the IDs of the
#' matched entries. The following columns contain the fields you have requested
#' through the `fields` parameter.
annotateMzValues=function(x, mz.tol, ms.mode, mz.tol.unit=c('plain', 'ppm'),
    mass.field='monoisotopic.mass', max.results=3, mz.col='mz', fields=NULL,
    prefix=NULL, insert.input.values=TRUE, fieldsLimit=0) {

    private$checkIsCompounddb()
    if (is.null(x))
        return(NULL)

    ret <- data.frame(stringsAsFactors=FALSE)
    newCols <- character()
    mz.tol.unit <- match.arg(mz.tol.unit)
    ef <- private$bdb$getEntryFields()
    mass.field <- match.arg(mass.field, ef$getFieldNames('mass'))

    # Convert x to data frame
    if ( ! is.data.frame(x))
        x <- data.frame(mz = x)

    # Check that we find the M/Z column
    if (nrow(x) > 0 && ! mz.col %in% names(x))
        error('No column named "%s" was found inside data frame.', mz.col)

    # Set M/Z col in output data frame
    if (mz.col %in% names(x))
        ret[[mz.col]] <- numeric()

    # Set output fields
    if ( ! is.null(fields))
        ef$checkIsDefined(fields)
    if (is.null(fields))
        fields <- self$getEntryIdField()

    # Set prefix
    if (is.null(prefix))
        prefix <- paste0(self$getId(), '.')

    # Get proton mass
    pm <- private$bdb$getConfig()$get('proton.mass')

    # Loop on all masses
    prg <- Progress$new(biodb=private$bdb, msg='Annotating M/Z values.',
                        total=nrow(x))
    for (i in seq_len(nrow(x))) {

        # Send progress message
        prg$increment()

        # Compute mass
        m <- x[i, mz.col] + pm * (if (ms.mode == 'neg') +1.0 else -1.0)

        # Search for compounds matching this mass
        rng <- Range$new(value=m, tol=mz.tol, tolType=mz.tol.unit)
        fieldsFilter <- list()
        fieldsFilter[[mass.field]] <- rng$getTolExpr()
        ids <- self$searchForEntries(fieldsFilter, max.results=max.results)

        # Get entries
        entries <- self$getEntry(ids, drop=FALSE)

        # Convert entries to data frame
        df <- private$bdb$entriesToDataframe(entries, fields=fields,
            limit=fieldsLimit)

        # Add prefix
        if ( ! is.null(df) && ncol(df) > 0 && ! is.na(prefix)
            && nchar(prefix) > 0) {
            fct <- function(x) substr(x, 1, nchar(prefix)) != prefix
            noprefix <- vapply(colnames(df), fct, FUN.VALUE=TRUE)
            colnames(df)[noprefix] <- paste0(prefix,
                colnames(df)[noprefix])
    }

        # Register new columns
        if ( ! is.null(df)) {
            c <- colnames(df)[ ! colnames(df) %in% newCols]
            newCols <- c(newCols, c)
        }

        # Insert input values
        if (insert.input.values)
            df <- if (is.null(df) || nrow(df) == 0) x[i, , drop=FALSE]
                else cbind(x[i, , drop=FALSE], df, row.names=NULL,
                    stringsAsFactors=FALSE)

        # Append local data frame to main data frame
        ret <- plyr::rbind.fill(ret, df)
    }

    # Sort new columns
    if ( ! is.null(ret)) {
        isAnInputCol <- ! colnames(ret) %in% newCols
        inputCols <- colnames(ret)[isAnInputCol]
        ret <- ret[, c(inputCols, sort(newCols)), drop=FALSE]
    }

    return(ret)
},

#' @description
#' Tests if the connector's database is a mass spectra database.
#' @return Returns TRUE if the database is a mass database.
isMassdb=function() {

    return(self$getPropertyValue('mass.db'))
},

#' @description
#' Checks that the database is correct by trying to retrieve all its
#'     entries.
#' @return Nothing.
checkDb=function() {
    # Get IDs
    ids <- self$getEntryIds()

    # Get entries
    entries <- private$bdb$getFactory()$getEntry(self$getId(), ids)

    return(invisible(NULL))
},

#' @description
#' Get all entries stored in the memory cache (volatile cache).
#' @return A list of BiodbEntry instances.
getAllVolatileCacheEntries=function() {

    # Remove NULL entries
    entries <- private$entries[ ! vapply(private$entries, is.null,
                                        FUN.VALUE=TRUE)]

    # Remove names
    names(entries) <- NULL

    return(entries)
},

#' @description
#' This method is deprecated.
#' Use getAllVolatileCacheEntries() instead.
#' @return All entries cached in memory.
getAllCacheEntries=function() {
    lifecycle::deprecate_soft('1.0.0', 'getAllCacheEntries()',
        "getAllVolatileCacheEntries()")
    return(self$getAllVolatileCacheEntries())
},

#' @description
#' Delete all entries from the volatile cache (memory cache).
#' @return Nothing.
deleteAllEntriesFromVolatileCache=function() {

    private$entries <- list()
    
    return(invisible(NULL))
},

#' @description
#' Delete all entries from the persistent cache (disk cache).
#' @param deleteVolatile If TRUE deletes also all entries from the volatile
#' cache (memory cache).
#' @return Nothing.
deleteAllEntriesFromPersistentCache=function(deleteVolatile=TRUE) {

    if (deleteVolatile)
        self$deleteAllEntriesFromVolatileCache()
    fileExt <- self$getPropertyValue('entry.content.type')
    private$bdb$getPersistentCache()$deleteFiles(self$getCacheId(),
        ext=fileExt)
    
    return(invisible(NULL))
},

#' @description
#' Delete all files associated with this connector from the persistent
#' cache (disk cache).
#' @param deleteVolatile If TRUE deletes also all entries
#' from the volatile cache (memory cache).
#' @return Nothing.
deleteWholePersistentCache=function(deleteVolatile=TRUE) {

    if (deleteVolatile)
        self$deleteAllEntriesFromVolatileCache()
    private$bdb$getPersistentCache()$deleteAllFiles(self$getCacheId())

    return(invisible(NULL))
},

#' @description
#' Delete all entries from the memory cache. This method is deprecated,
#' please use deleteAllEntriesFromVolatileCache() instead.
#' @return Nothing.
deleteAllCacheEntries=function() {
    lifecycle::deprecate_soft('1.0.0', 'deleteAllCacheEntries()',
        "deleteAllEntriesFromVolatileCache()")
    self$deleteAllEntriesFromVolatileCache()

    return(invisible(NULL))
},

#' @description
#' Gets the ID used by this connector in the disk cache.
#' @return The cache ID of this connector.
getCacheId=function() {

    id <- NULL

    if ( ! is.null(private$cache.id) && ! is.na(private$cache.id)) {
        id <- private$cache.id

    } else {
        url <- self$getPropValSlot('urls', 'base.url')
        if ( ! is.null(url) && ! is.na(url))
            id <- paste(self$getDbClass(), openssl::md5(url), sep='-')
    }

    return(id)
},

#' @description
#' Tests if some entry of this database makes reference to another entry of
#' another database.
#' @param id A character vector of entry IDs from the connector's database.
#' @param db Another database connector.
#' @param oid A entry ID from database db.
#' @param any If set to TRUE, returns a single logical value: TRUE if any entry
#' contains a reference to oid, FALSE otherwise.
#' @param recurse If set to TRUE, the algorithm will follow all references to
#' entries from other databases, to see if it can establish an indirect link to
#' `oid`.
#' @return A logical vector, the same size as `id`, with TRUE for each entry
#' making reference to `oid`, and FALSE otherwise.
makesRefToEntry=function(id, db, oid, any=FALSE, recurse=FALSE) {

    # Returns TRUE if any entry in id makes reference to oid
    if (any) {
        makes_ref <- FALSE
        for (i in id) {
            e <- self$getEntry(i)
            if ( ! is.null(e)
                && e$makesRefToEntry(db=db, oid=oid, recurse=recurse)) {
                makes_ref <- TRUE
                break
            }
        }
    }

    # Returns a vector, testing each entry in id individually
    else {
        entries <- self$getEntry(id, drop=FALSE)
        makes_ref <- vapply(entries,
            function(e) ! is.null(e) && e$makesRefToEntry(db=db, oid=oid,
            recurse=recurse), FUN.VALUE=TRUE)
    }
    return(makes_ref)
},

#' @description
#' Makes a BiodbRequest instance using the passed parameters, and set
#'     ifself as the associated connector.
#' @param ... Those parameters are passed to the initializer of BiodbRequest.
#' @return The BiodbRequest instance.
makeRequest=function(...) {

    req <- BiodbRequest$new(...)

    req$setConn(self)

    return(req)
},

#' @description
#' Gets the URL to a picture of the entry (e.g.: a picture of the
#'     molecule in case of a compound entry).
#' @param entry.id A character vector containing entry IDs.
#' @return A character vector, the same length as `entry.id`,
#'     containing for each entry ID either a URL or NA if no URL exists.
getEntryImageUrl=function(entry.id) {

    private$checkIsRemote()
    return(private$doGetEntryImageUrl(entry.id))
},

#' @description
#' Gets the URL to the page of the entry on the database web site.
#' @param entry.id A character vector with the IDs of entries to retrieve.
#' @return A list of BiodbUrl objects, the same length as `entry.id`.
getEntryPageUrl=function(entry.id) {

    private$checkIsRemote()
    return(private$doGetEntryPageUrl(entry.id))
},

#' @description
#' Gets a list of chromatographic columns contained in this database.
#' @param ids A character vector of entry identifiers (i.e.: accession
#' numbers).  Used to restrict the set of entries on which to run the
#' algorithm.
#' @return A data.frame with two columns, one for the ID 'id' and another one
#' for the title 'title'.
getChromCol=function(ids=NULL) {
    private$checkIsMassdb()
    return(private$doGetChromCol(ids))
},

#' @description
#' Gets the field to use for M/Z matching.
#' @return The name of the field (one of peak.mztheo or peak.mzexp).
getMatchingMzField=function() {
    
    private$checkIsMassdb()
    field <- NULL

    # Get value(s) defined in matching.fields property
    fields <- self$getPropValSlot('matching.fields', 'mz')
    
    # If it contains one value, return it
    if (length(fields) == 1)
        field <- fields
    
    # If it contains no value, throw an error
    else if (length(fields) == 0)
        error0("No macthing field defined for M/Z values.",
            "Use setMatchingMzField() to set one.")
    
    # If it contains more than one value, try to determine which one to use
    else {

        multiple.match <- FALSE

        # Get the parsing expressions and check which field is associated with
        # a parssing expression
        for (f in fields) {
            pars.expr <- self$getPropValSlot('parsing.expr', f)
            if ( ! is.null(pars.expr) && ! is.na(pars.expr)) {
                if (is.null(field))
                    field <- f
                else {
                    multiple.match <- TRUE
                    break
                }
            }
        }

        # Otherwise get an entry from the database and check what fields it
        # contains
        if (is.null(field) || multiple.match) {
            field.2 <- NULL
            multiple.match.2 <- FALSE
            id <- self$getEntryIds(max.results=1)
            if (length(id) == 1) {
                entry <- self$getEntry(id)
                for (f in fields)
                    if (entry$hasField(f)) {
                        if (is.null(field.2))
                            field.2 <- f
                        else {
                            multiple.match.2 <- TRUE
                            break
                        }
                    }
            }
            if ( ! is.null(field.2)) {
                field <- field.2
                multiple.match <- multiple.match.2
            }
        }
        
        # No choice made
        if (is.null(field))
            error0("Impossible to determine which field to use for",
                " M/Z matching. Please set the wanted field using",
                " setMatchingMzField() method, and make sure it is",
                " defined inside your database.")
        
        # Throw a warning telling which field was chosen for matching and tell
        # to use setMatchingMzField() to set another field if needed
        self$setMatchingMzField(field)
        if (multiple.match)
            warn0('Field "', field, '" has been automatically chosen',
                ' among several possibilities (',
                paste(fields, collapse=', '), ') for matching',
                ' M/Z values. Use setMatchingMzField() method',
                ' explicitly to avoid this warning in the future.')
    }
    
    return(field)
},

#' @description
#' Sets the field to use for M/Z matching.
#' @param field The field to use for matching.
#' @return Nothing.
setMatchingMzField=function(field=c('peak.mztheo', 'peak.mzexp')) {
    
    private$checkIsMassdb()
    field <- match.arg(field)
    
    self$setPropValSlot('matching.fields', 'mz', field)

    return(invisible(NULL))
},

#' @description
#' Gets a list of M/Z values contained inside the database.
#' @param ms.mode The MS mode. Set it to either 'neg' or 'pos' to limit the
#' output to one mode.
#' @param max.results If set, it is used to limit the size of the output.
#' @param precursor If set to TRUE, then restrict the search to precursor peaks.
#' @param ms.level The MS level to which you want to restrict your search.  0
#' means that you want to search in all levels.
#' @return A numeric vector containing M/Z values.
getMzValues=function(ms.mode=NULL, max.results=0, precursor=FALSE, ms.level=0) {

    private$checkIsMassdb()
    private$doGetMzValues(ms.mode=ms.mode, max.results=max.results,
        precursor=precursor, ms.level=ms.level)
},

#' @description
#' Gets the number of peaks contained in the database.
#' @param mode The MS mode. Set it to either 'neg' or 'pos' to limit the
#' counting to one mode.
#' @param ids A character vector of entry identifiers (i.e.: accession
#' numbers).  Used to restrict the set of entries on which to run the
#' algorithm.
#' @return The number of peaks, as an integer.
getNbPeaks=function(mode=NULL, ids=NULL) {
    private$checkIsMassdb()
    return(private$doGetNbPeaks(mode=mode, ids=ids))
},

#' @description
#' Filters a list of entries on retention time values.
#' @param entry.ids A character vector of entry IDs.
#' @param rt A vector of retention times to match. Used if input.df is not set.
#' Unit is specified by rt.unit parameter.
#' @param rt.unit The unit for submitted retention times. Either 's' or 'min'.
#' @param rt.tol The plain tolerance (in seconds) for retention times: input.rt
#' - rt.tol <= database.rt <= input.rt + rt.tol.
#' @param rt.tol.exp A special exponent tolerance for retention times: input.rt
#' - input.rt ** rt.tol.exp <= database.rt <= input.rt + input.rt **
#' rt.tol.exp. This exponent is applied on the RT value in seconds. If both
#' rt.tol and rt.tol.exp are set, the inequality expression becomes input.rt -
#' rt.tol - input.rt ** rt.tol.exp <= database.rt <= input.rt + rt.tol +
#' input.rt ** rt.tol.exp.
#' @param chrom.col.ids IDs of chromatographic columns on which to match the
#' retention time.
#' @param match.rt If set to TRUE, filters on RT values, otherwise does not do
#' any filtering.
#' @return A character vector containing entry IDs after filtering.
filterEntriesOnRt=function(entry.ids, rt, rt.unit, rt.tol, rt.tol.exp,
    chrom.col.ids, match.rt) {

    private$checkIsMassdb()
    private$checkRtParam(rt=rt, rt.unit=rt.unit, rt.tol=rt.tol,
        rt.tol.exp=rt.tol.exp, chrom.col.ids=chrom.col.ids, match.rt=match.rt)

    if (match.rt) {

        # Get entries
        logDebug('Getting entries from spectra IDs.')
        entries <- private$bdb$getFactory()$getEntry(self$getId(),
            entry.ids, drop=FALSE)

        # Filter on chromatographic columns
        if ( ! is.null(chrom.col.ids) && length(chrom.col.ids) > 0) {
            fct <- function(e) {
                e$getFieldValue('chrom.col.id') %in% chrom.col.ids
            }
            entries <- entries[vapply(entries, fct, FUN.VALUE=TRUE)]
            logDebug0(length(entries),
                ' spectra remaining after chrom col filtering: ',
                paste(vapply((if (length(entries) <= 10) entries
                else entries[seq_len(10)]),
                function(e) e$getFieldValue('accession'),
                FUN.VALUE=''), collapse=', '), '.')
        }

        # Filter out entries with no RT values or no RT unit
        fct <- function(e) {
            e$hasField('chrom.rt') || (e$hasField('chrom.rt.min')
                && e$hasField('chrom.rt.max'))
        }
        has.chrom.rt.values <- vapply(entries, fct, FUN.VALUE=TRUE)
        entries <- entries[has.chrom.rt.values]
        n <- sum( ! has.chrom.rt.values) > 0
        if (n > 0)
            logDebug('Filtered out %d entries having no RT values.', n)
        fct <- function(e) e$hasField('chrom.rt.unit')
        no.chrom.rt.unit <- ! vapply(entries, fct, FUN.VALUE=TRUE)
        if (any(no.chrom.rt.unit))
            warn0('No RT unit specified in entries ',
                paste(vapply(entries[no.chrom.rt.unit],
                function(e) e$getFieldValue('accession'),
                FUN.VALUE=''),
                collapse=', '),
                ', impossible to match retention times.')

        # Compute RT range for this input, in seconds
        rt.range <- private$computeRtRange(rt=rt, rt.unit=rt.unit,
            rt.tol=rt.tol, rt.tol.exp=rt.tol.exp)

        # Loop on all entries
        entry.ids <- character()
        for (e in entries) {

            # Get RT min and max for this column, in seconds
            col.rt.range <- private$computeChromColRtRange(e)

            # Test and possibly keep entry
            logDebug0('Testing if RT value ', rt, ' (', rt.unit,
                ') is in range [', col.rt.range$min, ';',
                col.rt.range$max, '] (s) of database entry ',
                e$getFieldValue('accession'), '. Used range (after',
                ' applying tolerances) for RT value is [', rt.range$min,
                ', ', rt.range$max, '] (s).')
            if ((rt.range$max >= col.rt.range$min)
                && (rt.range$min <= col.rt.range$max))
                entry.ids <- c(entry.ids, e$getFieldValue('accession'))
        }

        logDebug0(length(entry.ids),
            ' spectra remaining after retention time filtering:',
            paste((if (length(entry.ids) <= 10) entry.ids
            else entry.ids[seq_len(10)]), collapse=', '), '.')
    }

    return(entry.ids)
},

#' @description
#' Searches for entries (i.e.: spectra) that contain a peak around the given
#' M/Z value. Entries can also be filtered on RT values. You can input either a
#' list of M/Z values through mz argument and set a tolerance with mz.tol
#' argument, or two lists of minimum and maximum M/Z values through mz.min and
#' mz.max arguments.
#' @param mz A vector of M/Z values.
#' @param mz.tol The M/Z tolerance, whose unit is defined by mz.tol.unit.
#' @param mz.tol.unit The type of the M/Z tolerance. Set it to either to 'ppm'
#' or 'plain'.
#' @param mz.min A vector of minimum M/Z values.
#' @param mz.max A vector of maximum M/Z values. Its length must be the same as
#' `mz.min`.
#' @param rt A vector of retention times to match. Used if input.df is not set.
#' Unit is specified by rt.unit parameter.
#' @param rt.unit The unit for submitted retention times. Either 's' or 'min'.
#' @param rt.tol The plain tolerance (in seconds) for retention times: input.rt
#' - rt.tol <= database.rt <= input.rt + rt.tol.
#' @param rt.tol.exp A special exponent tolerance for retention times: input.rt
#' - input.rt ** rt.tol.exp <= database.rt <= input.rt + input.rt **
#' rt.tol.exp. This exponent is applied on the RT value in seconds. If both
#' rt.tol and rt.tol.exp are set, the inequality expression becomes input.rt -
#' rt.tol - input.rt ** rt.tol.exp <= database.rt <= input.rt + rt.tol +
#' input.rt ** rt.tol.exp.
#' @param chrom.col.ids IDs of chromatographic columns on which to match the
#' retention time.
#' @param precursor If set to TRUE, then restrict the search to precursor peaks.
#' @param min.rel.int The minimum relative intensity, in percentage (i.e.:
#' float number between 0 and 100).
#' @param ms.mode The MS mode. Set it to either 'neg' or 'pos'.
#' @param ms.level The MS level to which you want to restrict your search.  0
#' means that you want to search in all levels.
#' @param max.results If set, it is used to limit the number of matches found
#' for each M/Z value.
#' @param include.ids A list of IDs to which to restrict the final results. All
#' IDs that are not in this list will be excluded.
#' @return A character vector of spectra IDs.
searchForMassSpectra=function(mz.min=NULL, mz.max=NULL, mz=NULL, mz.tol=NULL,
mz.tol.unit=c('plain', 'ppm'), rt=NULL, rt.unit=c('s', 'min'), rt.tol=NULL,
rt.tol.exp=NULL, chrom.col.ids=NULL, precursor=FALSE, min.rel.int=0,
ms.mode=NULL, max.results=0, ms.level=0, include.ids=NULL) {

    private$checkIsMassdb()
    # Check arguments
    rt.unit <- match.arg(rt.unit)
    mz.tol.unit <- match.arg(mz.tol.unit)
    check.param <- private$checkSearchMsParam(mz.min=mz.min, mz.max=mz.max,
        mz=mz, mz.tol=mz.tol, mz.tol.unit=mz.tol.unit, rt=rt,
        rt.unit=rt.unit, rt.tol=rt.tol, rt.tol.exp=rt.tol.exp,
        chrom.col.ids=chrom.col.ids, min.rel.int=min.rel.int, ms.mode=ms.mode,
        max.results=max.results, ms.level=ms.level, match.rt=FALSE)
    if (is.null(check.param))
        return(NULL)

    ids <- character()

    if ((check.param$use.mz.min.max && ! all(is.na(mz.min) & is.na(mz.max)))
        || (check.param$use.mz.tol && ! all(is.na(mz)))) {

        if (check.param$use.rt.match) {
            # Search for one M/Z at a time
            for (i in seq_along(rt)) {

                # Search for this M/Z value
                if (check.param$use.mz.min.max)
                    mz.ids <- private$doSearchMzRange(mz.min=mz.min[[i]],
                        mz.max=mz.max[[i]], min.rel.int=min.rel.int,
                        ms.mode=ms.mode, max.results=0,
                        precursor=precursor, ms.level=ms.level)
                else
                    mz.ids <- private$doSearchMzTol(mz=mz[[i]], mz.tol=mz.tol,
                        mz.tol.unit=mz.tol.unit, min.rel.int=min.rel.int,
                        ms.mode=ms.mode, max.results=0,
                        precursor=precursor, ms.level=ms.level)

                # Filter on RT value
                rt.ids <- self$filterEntriesOnRt(mz.ids, rt=rt[[i]],
                    rt.unit=rt.unit, rt.tol=rt.tol, rt.tol.exp=rt.tol.exp,
                    chrom.col.ids=chrom.col.ids,
                    match.rt=check.param$use.rt.match)

                ids <- c(ids, rt.ids)
            }
        }

        else {
            # Search for all M/Z values
            if (check.param$use.mz.min.max)
                ids <- private$doSearchMzRange(mz.min=mz.min, mz.max=mz.max,
                    min.rel.int=min.rel.int, ms.mode=ms.mode,
                    max.results=max.results, precursor=precursor,
                    ms.level=ms.level)
            else
                ids <- private$doSearchMzTol(mz=mz, mz.tol=mz.tol,
                    mz.tol.unit=mz.tol.unit, min.rel.int=min.rel.int,
                    ms.mode=ms.mode, max.results=max.results,
                    precursor=precursor, ms.level=ms.level)
        }
    }

    # Remove duplicates
    ids <- ids[ ! duplicated(ids)]

    # Exclude IDs
    if ( ! is.null(include.ids))
        ids <- ids[ids %in% include.ids]

    # Cut
    if (max.results > 0 && length(ids) > max.results)
        ids <- ids[seq_len(max.results)]

    logDebug('Found %d spectra: %s', length(ids), lst2str(ids))
    return(ids)
},

#' @description
#' DEPRECATED. Use searchForMassSpectra() instead.
#' @param mz.min A vector of minimum M/Z values.
#' @param mz.max A vector of maximum M/Z values. Its length must be the same as
#' `mz.min`.
#' @param mz A vector of M/Z values.
#' @param mz.tol The M/Z tolerance, whose unit is defined by mz.tol.unit.
#' @param mz.tol.unit The type of the M/Z tolerance. Set it to either to 'ppm'
#' or 'plain'.
#' @param rt A vector of retention times to match. Used if input.df is not set.
#' Unit is specified by rt.unit parameter.
#' @param rt.unit The unit for submitted retention times. Either 's' or 'min'.
#' @param rt.tol The plain tolerance (in seconds) for retention times: input.rt
#' - rt.tol <= database.rt <= input.rt + rt.tol.
#' @param rt.tol.exp A special exponent tolerance for retention times: input.rt
#' - input.rt ** rt.tol.exp <= database.rt <= input.rt + input.rt **
#' rt.tol.exp. This exponent is applied on the RT value in seconds. If both
#' rt.tol and rt.tol.exp are set, the inequality expression becomes input.rt -
#' rt.tol - input.rt ** rt.tol.exp <= database.rt <= input.rt + rt.tol +
#' input.rt ** rt.tol.exp.
#' @param chrom.col.ids IDs of chromatographic columns on which to match the
#' retention time.
#' @param precursor If set to TRUE, then restrict the search to precursor peaks.
#' @param min.rel.int The minimum relative intensity, in percentage (i.e.:
#' float number between 0 and 100).
#' @param ms.mode The MS mode. Set it to either 'neg' or 'pos'.
#' @param ms.level The MS level to which you want to restrict your search.  0
#' means that you want to search in all levels.
#' @param max.results If set, it is used to limit the number of matches found
#' for each M/Z value.
#' @return A character vector of spectra IDs.
searchMsEntries=function(mz.min=NULL, mz.max=NULL, mz=NULL, mz.tol=NULL,
mz.tol.unit=c('plain', 'ppm'), rt=NULL, rt.unit=c('s', 'min'), rt.tol=NULL,
rt.tol.exp=NULL, chrom.col.ids=NULL, precursor=FALSE, min.rel.int=0,
ms.mode=NULL, max.results=0, ms.level=0) {
    lifecycle::deprecate_soft('1.0.0', 'searchMsEntries()',
        "searchForMassSpectra()")
    return(self$searchForMassSpectra(mz.min=mz.min, mz.max=mz.max, mz=mz,
        mz.tol=mz.tol, mz.tol.unit=mz.tol.unit, rt=rt, rt.unit=rt.unit,
        rt.tol= rt.tol, rt.tol.exp=rt.tol.exp, chrom.col.ids=chrom.col.ids,
        precursor=precursor, min.rel.int=min.rel.int, ms.mode=ms.mode,
        ms.level=ms.level, max.results=max.results))
},

#' @description
#' For each M/Z value, searches for matching MS spectra and returns the
#' matching peaks.
#' @param input.df A data frame taken as input for searchMsPeaks(). It must
#' contain a columns 'mz', and optionaly an 'rt' column.
#' @param mz A vector of M/Z values to match. Used if input.df is not set.
#' @param mz.tol The M/Z tolerance, whose unit is defined by mz.tol.unit.
#' @param mz.tol.unit The type of the M/Z tolerance. Set it to either to 'ppm'
#' or 'plain'.
#' @param min.rel.int The minimum relative intensity, in percentage (i.e.:
#' float number between 0 and 100).
#' @param ms.mode The MS mode. Set it to either 'neg' or 'pos'.
#' @param ms.level The MS level to which you want to restrict your search.  0
#' means that you want to search in all levels.
#' @param max.results If set, it is used to limit the number of matches found
#' for each M/Z value.
#' @param chrom.col.ids IDs of chromatographic columns on which to match the
#' retention time.
#' @param rt A vector of retention times to match. Used if input.df is not set.
#' Unit is specified by rt.unit parameter.
#' @param rt.unit The unit for submitted retention times. Either 's' or 'min'.
#' @param rt.tol The plain tolerance (in seconds) for retention times: input.rt
#' - rt.tol <= database.rt <= input.rt + rt.tol.
#' @param rt.tol.exp A special exponent tolerance for retention times: input.rt
#' - input.rt ** rt.tol.exp <= database.rt <= input.rt + input.rt **
#' rt.tol.exp. This exponent is applied on the RT value in seconds. If both
#' rt.tol and rt.tol.exp are set, the inequality expression becomes input.rt -
#' rt.tol - input.rt ** rt.tol.exp <= database.rt <= input.rt + rt.tol +
#' input.rt ** rt.tol.exp.
#' @param precursor If set to TRUE, then restrict the search to precursor peaks.
#' @param precursor.rt.tol The RT tolerance used when matching the precursor.
#' @param insert.input.values Insert input values at the beginning of the
#' result data frame.
#' @param prefix Add prefix on column names of result data frame.
#' @param compute If set to TRUE, use the computed values when converting found
#' entries to data frame.
#' @param fields A character vector of field names to output. The data frame
#' output will be restricted to this list of fields.
#' @param fieldsLimit The maximum of values to output for fields with multiple
#' values. Set it to 0 to get all values.
#' @param input.df.colnames Names of the columns in the input data frame.
#' @param match.rt If set to TRUE, match also RT values.
#' @return A data frame with at least input MZ and RT columns, and annotation
#' columns prefixed with `prefix` if set. For each matching found a row is
#' output. Thus if n matchings are found for M/Z value x, then there will be n
#' rows for x, each for a different match. The number of matching found for
#' each M/Z value is limited to `max.results`.
searchMsPeaks=function(input.df=NULL, mz=NULL, mz.tol=NULL,
mz.tol.unit=c('plain', 'ppm'), min.rel.int=0, ms.mode=NULL, ms.level=0,
max.results=0, chrom.col.ids=NULL, rt=NULL, rt.unit=c('s', 'min'), rt.tol=NULL,
rt.tol.exp=NULL, precursor=FALSE, precursor.rt.tol=NULL,
insert.input.values=TRUE, prefix=NULL, compute=TRUE, fields=NULL,
fieldsLimit=0, input.df.colnames=c(mz='mz', rt='rt'), match.rt=FALSE) {

    # Checks
    private$checkIsMassdb()
    rt.unit <- match.arg(rt.unit)
    mz.tol.unit <- match.arg(mz.tol.unit)
    check.param <- do.call(private$checkSearchMsParam, as.list(environment()))
    if (is.null(check.param))
        return(NULL)
    input.df <- check.param$input.df

    precursor.match.ids <- NULL

    # Step 1 - search for spectra with specified precursor
    if (precursor)
        precursor.match.ids <- self$searchForMassSpectra(mz.min=NULL,
            mz.max=NULL, mz=input.df[[input.df.colnames[['mz']]]],
            mz.tol=mz.tol, mz.tol.unit=mz.tol.unit,
            rt=input.df[[input.df.colnames[['rt']]]], rt.unit=rt.unit,
            rt.tol=precursor.rt.tol, chrom.col.ids=chrom.col.ids,
            precursor=precursor, min.rel.int=min.rel.int, ms.mode=ms.mode,
            ms.level=ms.level)

    # Step 2 - search for matching spectra for each M/Z value
    results <- private$matchingMzWithSpectra(input.df=input.df,
        input.df.colnames=input.df.colnames, min.rel.int=min.rel.int,
        ms.mode=ms.mode, ms.level=ms.level, max.results=max.results,
        mz.tol=mz.tol, mz.tol.unit=mz.tol.unit,
        rt=rt, rt.unit=rt.unit, rt.tol=rt.tol, rt.tol.exp=rt.tol.exp,
        chrom.col.ids=chrom.col.ids, match.rt=check.param$use.rt.match,
        fields=fields, compute=compute, prefix=prefix, fieldsLimit=fieldsLimit,
        insert.input.values=insert.input.values,
        precursor.match.ids=precursor.match.ids)

    return(results)
},

#' @description
#' Searches MSMS spectra matching a template spectrum. The mz.tol
#'     parameter is applied on the precursor search.
#' @param spectrum A template spectrum to match inside the database.
#' @param precursor.mz The M/Z value of the precursor peak of the mass spectrum.
#' @param mz.tol The M/Z tolerance, whose unit is defined by mz.tol.unit.
#' @param mz.tol.unit The type of the M/Z tolerance. Set it to either to
#' 'ppm' or 'plain'.
#' @param ms.mode The MS mode. Set it to either 'neg' or 'pos'.
#' @param npmin The minimum number of peak to detect a match (2 is recommended).
#' @param dist.fun The distance function used to compute the distance
#' betweem two mass spectra.
#' @param msms.mz.tol M/Z tolerance to apply while matching MSMS spectra.
#' In PPM.
#' @param msms.mz.tol.min Minimum of the M/Z tolerance (plain unit). If
#' the M/Z tolerance computed with `msms.mz.tol` is lower than
#' `msms.mz.tol.min`, then `msms.mz.tol.min` will be used.
#' @param max.results If set, it is used to limit the number of matches
#' found for each M/Z value.
#' @return A data frame with columns `id`, `score` and `peak.*`. Each
#' `peak.*` column corresponds to a peak in the input spectrum, in the
#' same order and gives the number of the peak that was matched with it
#' inside the matched spectrum whose ID is inside the `id` column.
msmsSearch=function(spectrum, precursor.mz, mz.tol, mz.tol.unit=c('plain',
    'ppm'), ms.mode, npmin=2, dist.fun=c('wcosine', 'cosine', 'pkernel',
    'pbachtttarya'), msms.mz.tol=3, msms.mz.tol.min=0.005, max.results=0) {

    private$checkIsMassdb()
    peak.tables <- list()
    dist.fun <- match.arg(dist.fun)
    mz.tol.unit <- match.arg(mz.tol.unit)
    chk::chk_number(max.results)
    chk::chk_gte(max.results, 0)

    # Get spectra IDs
    ids <- character()
    if ( ! is.null(spectrum) && nrow(spectrum) > 0 && ! is.null(precursor.mz)) {
        if (max.results > 0)
            warn0('Applying max.results =', max.results,'on call to',
                ' searchForMassSpectra(). This may results in no matches,',
                ' while there exist matching spectra inside the database.')
        ids <- self$searchForMassSpectra(mz=precursor.mz, mz.tol=mz.tol,
            mz.tol.unit=mz.tol.unit, ms.mode=ms.mode, precursor=TRUE,
            ms.level=2, max.results=max.results)
    }

    # Get list of peak tables from spectra
    if (length(ids) > 0) {
        entries <- private$bdb$getFactory()$getEntry(self$getId(), ids,
            drop=FALSE)
        fct <- function(x) {
            x$getFieldsAsDataframe(only.atomic=FALSE, flatten=FALSE,
                fields=c('peak.mz', 'peak.relative.intensity',
                'peak.intensity'))
        }
        peak.tables <- lapply(entries, fct)
    }

    # Compare spectrum against database spectra
    res <- compareSpectra(spectrum, peak.tables, npmin=npmin, fun=dist.fun,
        params=list(ppm=msms.mz.tol, dmz=msms.mz.tol.min))
    
    cols <- colnames(res)
    res[['id']] <- ids
    res <- res[, c('id', cols)]
    
    # Order rows
    res <- res[order(res[['score']], decreasing=TRUE), ]
    
    return(res)
},

#' @description
#' Collapse rows of a results data frame, by outputing a data frame with
#'     only one row for each MZ/RT value.
#' @param results.df Results data frame.
#' @param  mz.col The name of the M/Z column in the results data frame.
#' @param  rt.col The name of the RT column in the results data frame.
#' @param  sep The separator used to concatenate values, when collapsing results
#'     data frame.
#' @return A data frame with rows collapsed."
collapseResultsDataFrame=function(results.df, mz.col='mz', rt.col='rt',
    sep='|') {
    private$checkIsMassdb()
    cols <- mz.col
    if (rt.col %in% colnames(results.df))
        cols <- c(cols, rt.col)
    x <- private$bdb$collapseRows(results.df, cols=cols)
    
    return(x)
},

#' @description
#' Find spectra in the given M/Z range. Returns a list of spectra IDs.
#' @param mz.min A vector of minimum M/Z values.
#' @param mz.max A vector of maximum M/Z values. Its length must be the
#' same as `mz.min`.
#' @param min.rel.int The minimum relative intensity, in percentage (i.e.:
#' float number between 0 and 100).
#' @param ms.mode The MS mode. Set it to either 'neg' or 'pos'.
#' @param ms.level The MS level to which you want to restrict your search.
#' 0 means that you want to search in all levels.
#' @param max.results If set, it is used to limit the number of matches
#' found for each M/Z value.
#' @param precursor If set to TRUE, then restrict the search to precursor
#' peaks.
#' @return A character vector of spectra IDs.
searchMzRange=function(mz.min, mz.max, min.rel.int=0, ms.mode=NULL,
    max.results=0, precursor=FALSE, ms.level=0) {
    lifecycle::deprecate_soft('1.0.0', 'searchMzRange()',
        'searchForMassSpectra()')

    return(self$searchForMassSpectra(mz.min=mz.min, mz.max=mz.max,
        min.rel.int=min.rel.int, ms.mode=ms.mode, max.results=max.results,
        precursor=precursor, ms.level=ms.level))
},

#' @description
#' Find spectra containg a peak around the given M/Z value. Returns a
#' character vector of spectra IDs.
#' @param mz A vector of M/Z values.
#' @param mz.tol The M/Z tolerance, whose unit is defined by mz.tol.unit.
#' @param mz.tol.unit The type of the M/Z tolerance. Set it to either to
#' 'ppm' or 'plain'.
#' @param min.rel.int The minimum relative intensity, in percentage (i.e.:
#' float number between 0 and 100).
#' @param ms.mode The MS mode. Set it to either 'neg' or 'pos'.
#' @param ms.level The MS level to which you want to restrict your search.
#' 0 means that you want to search in all levels.
#' @param max.results If set, it is used to limit the number of matches
#' found for each M/Z value.
#' @param precursor If set to TRUE, then restrict the search to precursor
#' peaks.
#' @return A character vector of spectra IDs.
searchMzTol=function(mz, mz.tol, mz.tol.unit='plain', min.rel.int=0,
    ms.mode=NULL, max.results=0, precursor=FALSE, ms.level=0) {
    lifecycle::deprecate_soft('1.0.0', 'searchMzTol()',
        'searchForMassSpectra()')
    
    return(self$searchForMassSpectra(mz=mz, mz.tol=mz.tol,
        mz.tol.unit=mz.tol.unit, min.rel.int=min.rel.int, ms.mode=ms.mode,
        max.results=max.results, precursor=precursor, ms.level=ms.level))
}
),

private=list(
    id=NULL,
    entries=NULL,
    cache.id=NULL,
    editing.allowed=NULL,
    writing.allowed=NULL,
    bdb=NULL

,doHasField=function(field) {
    # TODO Should check which entry fields are parsed in general.
    # TODO For that, even fields dynamically computed should be declared in
    # definitions.yml file.
    return(TRUE)
}

,doesRequireDownload=function() {
    return(FALSE)
}

,doCorrectIds=function(ids) {
    return(ids)
}

,doGetNbEntries=function(count=FALSE) {

    n <- NA_integer_

    if (count) {
        ids <- self$getEntryIds()
        if ( ! is.null(ids))
            n <- length(ids)
    }

    return(n)
},

doGetEntryPageUrl=function(id) {
    abstractMethod(self)
},

doGetEntryImageUrl=function(id) {
    return(rep(NA_character_, length(id)))
},

checkIsEditable=function() {
    if ( ! self$isEditable())
        error0("The database associated to this connector ", self$getId(),
            " is not editable.")
},

initEditable=function() {
    if (length(private$editing.allowed) == 0)
        self$setEditingAllowed(FALSE)
},

checkEditingIsAllowed=function() {

    private$initEditable()

    if ( ! private$editing.allowed)
        error0('Editing is not enabled for this database. However this',
            ' database type is editable. Please call allowEditing()',
            ' method to enable editing.')
},

checkIsWritable=function() {
    if ( ! self$isWritable())
        error0("The database associated to this connector ", self$getId(),
            " is not writable.")
},

checkWritingIsAllowed=function() {
    
    private$initWritable()
    
    if ( ! private$writing.allowed)
        error0('Writing is not enabled for this database. However this',
            ' database type is writable. Please call allowWriting()',
            ' method to enable writing.')
},

doWrite=function() {
    abstractMethod(self)
},

initWritable=function() {
    if (length(private$writing.allowed) == 0)
        self$setWritingAllowed(FALSE)
},

doSearchForEntries=function(fields=NULL, max.results=0) {
    # To be implemented by derived class.
    return(NULL)
},

checkIsDownloadable=function() {
    if ( ! self$isDownloadable())
        error0("The database associated to this connector ", self$getId(),
            " is not downloadable.")
},

doDownload=function() {
    abstractMethod(self)
},

doExtractDownload=function() {
    abstractMethod(self)
},

checkIsRemote=function() {
    if ( ! self$isRemotedb())
        error0("The database associated to this connector ", self$getId(),
            " is not a remote database.")
},

checkIsCompounddb=function() {
    if ( ! self$isCompounddb())
        error0("The database associated to this connector ", self$getId(),
            " is not a compound database.")
},

checkMassField=function(mass, mass.field) {

    if ( ! is.null(mass)) {
        chk::chk_number(mass)
        chk::chk_string(mass.field)
        ef <- private$bdb$getEntryFields()
        mass.fields <- ef$getFieldNames(type='mass')
        chk::chk_subset(mass.field, mass.fields)
    }
},

checkIsMassdb=function() {
    if ( ! self$isMassdb())
        error0("The database associated to this connector ", self$getId(),
            " is not a mass spectra database.")
},

doGetEntryContentRequest=function(id, concatenate=TRUE) {
    private$checkIsRemote()
    abstractMethod(self)
},

doGetEntryContentOneByOne=function(entry.id) {

    private$checkIsRemote()

    # Initialize return values
    content <- rep(NA_character_, length(entry.id))

    # Get requests
    requests <- self$getEntryContentRequest(entry.id, concatenate=FALSE)

    # Get encoding
    encoding <- self$getPropertyValue('entry.content.encoding')

    # If requests is a vector of characters, then the method is using the old
    # scheme.
    # We now convert the requests to the new scheme, using class BiodbRequest.
    if (is.character(requests)) {
        fct <- function(x) self$makeRequest(method='get', url=BiodbUrl$new(x),
            encoding=encoding)
        requests <- lapply(requests, fct)
    }

    # Send requests
    scheduler <- private$bdb$getRequestScheduler()
    prg <- Progress$new(biodb=private$bdb,
                        msg='Downloading entry contents',
                        total=length(requests))
    for (i in seq_along(requests)) {
        prg$increment()
        content[[i]] <- scheduler$sendRequest(requests[[i]])
    }

    return(content)
},

doGetEntryIds=function(max.results=0) {
    abstractMethod(self)
},


doGetEntryContentFromDb=function(id) {

    if (self$isRemotedb())
        return(private$doGetEntryContentOneByOne(id))

    abstractMethod(self)
},

doGetChromCol=function(ids=NULL) {
    abstractMethod(self)
},

doGetNbPeaks=function(mode=NULL, ids=NULL) {
    abstractMethod(self)
},

addEntriesToCache=function(ids, entries) {

    ids <- as.character(ids)

    names(entries) <- ids

    # Update known entries
    known.ids <- ids[ids %in% names(private$entries)] 
    private$entries[known.ids] <- entries[ids %in% known.ids]

    # Add new entries
    new.ids <- ids[ ! ids %in% names(private$entries)]
    private$entries <- c(private$entries, entries[ids %in% new.ids])
},

getEntriesFromCache=function(ids) {

    ids <- as.character(ids)

    return(private$entries[ids])
},

getEntryMissingFromCache=function(ids) {

    ids <- as.character(ids)

    missing.ids <- ids[ ! ids %in% names(private$entries)]

    return(missing.ids)
},

doSearchMzTol=function(mz, mz.tol, mz.tol.unit, min.rel.int, ms.mode,
    max.results, precursor, ms.level) {

    rng <- convertTolToRange(mz, tol=mz.tol, type=mz.tol.unit)

    return(self$searchForMassSpectra(mz.min=rng$a, mz.max=rng$b,
        min.rel.int=min.rel.int, ms.mode=ms.mode, max.results=max.results,
        precursor=precursor, ms.level=ms.level))
},

doSearchMzRange=function(mz.min, mz.max, min.rel.int, ms.mode,
    max.results, precursor, ms.level) {
    abstractMethod(self)
},

doGetMzValues=function(ms.mode, max.results, precursor, ms.level) {
    abstractMethod(self)
},

convertRt=function(rt, units, wanted.unit) {

    # RT values with wrong unit
    rt.wrong <- units != wanted.unit

    # Convert any RT value using wrong unit
    if (any(rt.wrong)) {
        if ('s' %in% units[rt.wrong]) {
            if (wanted.unit != 'min')
                error0('Error when converting retention times values.',
                    ' Was expecting "min" for target unit.')
            rt[rt.wrong] <- rt[rt.wrong] / 60
        }
        if ('min' %in% units[rt.wrong]) {
            if (wanted.unit != 's')
                error0('Error when converting retention times values.',
                    ' Was expecting "s" for target unit.')
            rt[rt.wrong] <- rt[rt.wrong] * 60
        }
    }

    return(rt)
},

checkMzMinMaxParam=function(mz.min, mz.max) {

    use.min.max <- ! is.null(mz.min) && ! is.null(mz.max)

    if (use.min.max) {
        chk::chk_numeric(mz.min)
        chk::chk_numeric(mz.max)
        chk::chk_gte(mz.min, 0)
        chk::chk_gte(mz.max, 0)
        chk::chk_true(length(mz.min) == length(mz.max))
        chk::chk_lte(mz.min, mz.max)
    }

    return(use.min.max)
},

checkMzTolParam=function(mz, mz.tol, mz.tol.unit=c('ppm', 'plain')) {

    use.tol <- ! is.null(mz)

    if (use.tol) {
        chk::chk_numeric(mz)
        chk::chk_gte(mz, 0)
        chk::chk_number(mz.tol)
        chk::chk_gte(mz.tol, 0)
        mz.tol.unit <- match.arg(mz.tol.unit)
    }

    return(use.tol)
},

checkMzParam=function(mz.min, mz.max, mz, mz.tol, mz.tol.unit) {

    use.tol <- private$checkMzTolParam(mz=mz, mz.tol=mz.tol,
        mz.tol.unit=mz.tol.unit)
    use.min.max <- private$checkMzMinMaxParam(mz.min=mz.min, mz.max=mz.max)

    if (use.tol && use.min.max)
        error0("You cannot set both mz and (mz.min, mz.max). Please",
            " choose one of those these two schemes to input M/Z values.")

    return(list(use.tol=use.tol, use.min.max=use.min.max))
}

,checkRtParam=function(rt, rt.unit=c('s', 'min'), rt.tol, rt.tol.exp,
    chrom.col.ids, match.rt) {

    if (match.rt) {
        chk::chk_numeric(rt)
        chk::chk_gte(rt, 0)
        chk::chk_number(rt.tol)
        chk::chk_gte(rt.tol, 0)
        chk::chk_null_or(rt.tol.exp, vld=chk::vld_number)
        chk::chk_gte(rt.tol.exp, 0)
        chk::chk_null_or(chrom.col.ids, vld=chk::vld_character)
        chk::chk_null_or(chrom.col.ids, vld=chk::vld_not_any_na)
        rt.unit <- match.arg(rt.unit)
    }
}

,checkSearchMsParam=function(input.df=NULL, input.df.colnames=c(mz='mz',
    rt='rt', mz.min='mz.min', mz.max='mz.max'), mz.min=NULL, mz.max=NULL, mz,
    mz.tol, mz.tol.unit, rt, rt.unit, rt.tol, rt.tol.exp, chrom.col.ids,
    min.rel.int, ms.mode, max.results, ms.level, match.rt, ...) {

    match.rt <- match.rt || ! is.null(rt)

    # Set M/Z and RT input values
    if ( ! is.null(input.df)) {
        if (is.vector(input.df)) {
            input.df <- data.frame(mz=input.df)
            if ( ! 'mz' %in% colnames(input.df.colnames))
                input.df.colnames[['mz']] <- 'mz'
            colnames(input.df) <- input.df.colnames[['mz']]
        }
        chk::chk_is(input.df, 'data.frame')
        for (v in c('mz', 'mz.min', 'mz.max', 'rt')) {
            if (is.null(get(v)) && v %in% names(input.df.colnames)
                && ! is.null(input.df.colnames[[v]])
                && ! is.na(input.df.colnames[[v]])
                && input.df.colnames[[v]] %in% colnames(input.df))
                assign(v, input.df[[input.df.colnames[[v]]]])
        }
    }

    mz.match <- private$checkMzParam(mz.min=mz.min, mz.max=mz.max, mz=mz,
        mz.tol=mz.tol, mz.tol.unit=mz.tol.unit)
    private$checkRtParam(rt=rt, rt.unit=rt.unit, rt.tol=rt.tol,
        rt.tol.exp=rt.tol.exp, chrom.col.ids=chrom.col.ids, match.rt=match.rt)
    if ( ! mz.match$use.tol && ! mz.match$use.min.max)
        return(NULL)
    if (mz.match$use.tol && match.rt && length(mz) != length(rt))
        error0('mz and rt must have the same length.')
    if (mz.match$use.min.max && match.rt && length(mz.min) != length(rt))
        error0('mz.min, mz.max and rt must have the same length.')

    # Set input data frame
    for (v in c('mz', 'mz.min', 'mz.max', 'rt')) {
        if ( ! is.null(get(v))) {
            if (is.null(input.df)) {
                input.df <- data.frame(x=get(v))
                colnames(input.df) <- v
            } else {
                if (nrow(input.df) != length(get(v)))
                    error0('input.df (length ', nrow(input.df), '), and ',
                        v, ' (length ', length(get(v)),
                        ') must have the same length.')
                else {
                    if ( ! v %in% names(input.df.colnames))
                        input.df.colnames[[v]] <- v
                    input.df[[input.df.colnames[[v]]]] <- get(v)
                }
            }
        }
    }

    chk::chk_null_or(min.rel.int, vld=chk::vld_number)
    if ( ! is.null(min.rel.int))
        chk::chk_gte(min.rel.int, 0)
    ef <- private$bdb$getEntryFields()
    if ( ! is.null(ms.mode)) {
        chk::chk_subset(ms.mode, ef$get('ms.mode')$getAllowedValues())
        ms.mode <- ef$get('ms.mode')$correctValue(ms.mode)
    }
    chk::chk_number(max.results)
    chk::chk_gte(max.results, 0)
    chk::chk_number(ms.level)
    chk::chk_gte(ms.level, 0)

    return(list(use.mz.tol=mz.match$use.tol,
        use.mz.min.max=mz.match$use.min.max, use.rt.match=match.rt,
        input.df=input.df))
},

computeChromColRtRange=function(entry) {

    rt.col.unit <- entry$getFieldValue('chrom.rt.unit')

    if (entry$hasField('chrom.rt')) {
        rt.col.min <- private$convertRt(entry$getFieldValue('chrom.rt'),
            rt.col.unit, 's')
        rt.col.max <- rt.col.min
    } else if (entry$hasField('chrom.rt.min')
        && entry$hasField('chrom.rt.max')) {
        rt.col.min <- private$convertRt(entry$getFieldValue('chrom.rt.min'),
            rt.col.unit, 's')
        rt.col.max <- private$convertRt(entry$getFieldValue('chrom.rt.max'),
            rt.col.unit, 's')
    } else
        error0('Impossible to match on retention time, no retention time',
            ' fields (chrom.rt or chrom.rt.min and chrom.rt.max) were found.')

    return(list(min=rt.col.min, max=rt.col.max))
}

,computeRtRange=function(rt, rt.unit, rt.tol, rt.tol.exp) {

    rt.sec <- private$convertRt(rt, rt.unit, 's')
    rt.min <- rt.sec
    rt.max <- rt.sec
    logDebug('At step 1, RT range is [%g, %g] (s).', rt.min, rt.max)
    if ( ! is.na(rt.tol)) {
        logDebug('RT tol is %g (s).', rt.tol)
        rt.min <- rt.min - rt.tol
        rt.max <- rt.max + rt.tol
    }
    logDebug('At step 2, RT range is [%g, %g] (s).', rt.min, rt.max)
    if ( ! is.null(rt.tol.exp)) {
        logDebug('RT tol exp is %g.', rt.tol.exp)
        rt.min <- rt.min - rt.sec ** rt.tol.exp
        rt.max <- rt.max + rt.sec ** rt.tol.exp
    }
    logDebug('At step 3, RT range is [%g, %g] (s).', rt.min, rt.max)

    return(list(min=rt.min, max=rt.max))
}

,matchingMzWithSpectra=function(input.df, input.df.colnames, min.rel.int,
    ms.mode, ms.level, max.results, mz.tol, mz.tol.unit,
    rt, rt.unit, rt.tol, rt.tol.exp, chrom.col.ids, match.rt,
    fields, compute, prefix, fieldsLimit, insert.input.values,
    precursor.match.ids) {

    results <- NULL
    result.columns <- character()

    logDebug('M/Z values to process %s',
        lst2str(input.df[[input.df.colnames[['mz']]]]))
    for (i in seq_along(input.df[[input.df.colnames[['mz']]]])) {

        # Compute M/Z range
        mz <- input.df[i, input.df.colnames[['mz']]]
        rng <- convertTolToRange(mz, tol=mz.tol, type=mz.tol.unit)

        # Search for spectra
        prm <- list(mz.min=rng$a, mz.max=rng$b,
            min.rel.int=min.rel.int, ms.mode=ms.mode, ms.level=ms.level,
            max.results=if (match.rt) 0 else max.results,
            include.ids=precursor.match.ids)
        if  (match.rt)
            prm <- c(prm, list(rt=input.df[i, input.df.colnames[['rt']]],
                rt.unit=rt.unit, rt.tol=rt.tol, rt.tol.exp=rt.tol.exp,
                chrom.col.ids=chrom.col.ids))
        ids <- do.call(self$searchForMassSpectra, prm)

        # Get entries & data frame
        entries <- private$bdb$getFactory()$getEntry(self$getId(), ids,
            drop=FALSE, no.null=TRUE, limit=max.results)
        x <- private$convertEntriesIntoDataframe(entries=entries,
            compute=compute, fieldsLimit=fieldsLimit, mz.tol=mz.tol,
            mz.tol.unit=mz.tol.unit, rng=rng, fields=fields, prefix=prefix)

        # Register result columns
        if ( ! is.null(x)) {
            newCols <- colnames(x)[ ! colnames(x) %in% result.columns]
            result.columns <- c(result.columns, newCols)
        }

        results <- private$appendDataframeToResults(x=x, results=results,
            insert.input.values=insert.input.values,
            input.row=input.df[i, , drop=FALSE])
    }

    results <- private$sortResultCols(results=results,
        result.columns=result.columns)
    return(results)
}

,convertEntriesIntoDataframe=function(entries, compute, fieldsLimit,
    mz.tol, mz.tol.unit, fields, prefix, rng) {

    # Convert to data frame
    x <- private$bdb$entriesToDataframe(entries,
        only.atomic=FALSE, compute=compute, flatten=FALSE,
        limit=fieldsLimit)

    # Select lines with right M/Z values
    x <- x[(x$peak.mz >= rng$a) & (x$peak.mz <= rng$b), ]

    # Select fields
    if ( ! is.null(fields))
        x <- x[fields[fields %in% colnames(x)]]

    # Add prefix on column names
    if ( ! is.null(x) && ncol(x) > 0 && ! is.null(prefix)
        && ! is.na(prefix))
        colnames(x) <- paste0(prefix, colnames(x))

    return(x)
}

,appendDataframeToResults=function(x, results, insert.input.values, input.row) {

    # Inserting input values at the beginning of the data frame
    if (insert.input.values) {
        x <- if (is.null(x) || nrow(x) == 0) input.row
            else cbind(input.row, x, row.names=NULL, stringsAsFactors=FALSE)
    }

    # Appending to main results data frame
    results <- plyr::rbind.fill(results, x)

    return(results)
}

,sortResultCols=function(results, result.columns) {
    # Sort result columns. We sort at the end of the processing, because result
    # data frames may contain different number of column, depending on the
    # presence of NA values.
    # Sort cols at the end because of possible presence of NA values.
    if ( ! is.null(results)) {
        isAnInputCol <- ! colnames(results) %in% result.columns
        inputCols <- colnames(results)[isAnInputCol]
        results <- results[, c(inputCols, sort(result.columns)), drop=FALSE]
    }

    return(results)
}

,terminate=function() {

    # Unregister from the request scheduler
    if (self$isRemotedb()) {
        logDebug("Unregister connector %s from the request scheduler",
            self$getId())
        private$bdb$getRequestScheduler()$unregisterConnector(self)
    }
}
))
