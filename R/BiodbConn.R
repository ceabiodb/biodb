#' The mother abstract class of all database connectors.
#'
#' This is the super class of all connector classes. All methods defined here
#' are thus common to all connector classes. Some connector classes inherit
#' directly from this abstract class. Some others inherit from intermediate
#' classes \code{\link{BiodbCompounddbConn}} or \code{\link{BiodbMassdbConn}}.
#' As for all connector concrete classes, you won't have to create an instance
#' of this class directly, but you will instead go through the factory class.
#' However, if you plan to develop a new connector, you will have to call the
#' constructor of this class. See section Fields for a list of the constructor's
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
#' @seealso Super class \code{\link{BiodbConnBase}}, \code{\link{BiodbFactory}},
#' \code{\link{BiodbCompoundbConn}} and \code{\link{BiodbMassdbConn}}.
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
#' @import methods
#' @import openssl
#' @include BiodbConnBase.R
#' @export BiodbConn
#' @exportClass BiodbConn
BiodbConn <- methods::setRefClass("BiodbConn",
    contains="BiodbConnBase",
    fields=list(
        .id="character",
        .entries="list",
        .cache.id='character',
        .editing.allowed='logical',
        .writing.allowed='logical'
    ),

methods=list(

initialize=function(id=NA_character_, cache.id=NA_character_, ...) {

    callSuper(...)
    .self$.abstractClass('BiodbConn')

    chk::chk_character(id)
    .self$.id <- id
    .self$.cache.id <- if (is.null(cache.id)) NA_character_ else cache.id
    .self$.entries <- list()

    # Register with request scheduler
    if (.self$isRemotedb())
        .self$getBiodb()$getRequestScheduler()$.registerConnector(.self)
},

getId=function() {
    ":\n\nGet the identifier of this connector.
    \nReturned value: The identifier of this connector.
    "

    return(.self$.id)
},

show=function() {
    ":\n\nPrints a description of this connector.
    \nReturned value: None.
    "
    
    callSuper() 
    cat("  ID: ", .self$.id, ".\n", sep='')
},

correctIds=function(ids) {
    ":\n\nCorrect a vector of IDs by formatting them to the database official
    format, if required and possible.
    \nids: A character vector of IDs.
    \nReturned values: The vector of IDs corrected.
    "

    return(ids)
},

getEntry=function(id, drop=TRUE, nulls=TRUE) {
    ":\n\nReturn the entry corresponding to this ID. You can pass a vector of
    IDs, and you will get a list of entries.
    \nid: A character vector containing entry identifiers.
    \ndrop: If set to TRUE and only one entry is requrested, then the returned
    value will be a single BiodbEntry object, otherwise it will be a list of 
    BiodbEntry objects.
    \nnulls: If set to TRUE, NULL entries are preserved. This ensures that the
    output list has the same length than the input vector `id`. Otherwise they
    are removed from the final list.
    \nReturned value: A list of BiodbEntry objects, the same size of the vector
    of IDs. The list will contain NULL values for invalid IDs. If drop is set to
    TRUE and only one etrny was requested then a single BiodbEntry is returned
    instead of a list.
    "

    entries <- .self$getBiodb()$getFactory()$getEntry(.self$getId(), id=id,
        drop=drop)

    if ( ! nulls && is.list(entries))
        entries <- Filter(function(e) ! is.null(e), entries)

    return(entries)
},

getCacheFile=function(entry.id) {
    ":\n\nGet the path to the persistent cache file.
    \nentry.id: The identifiers (e.g.: accession numbers) as a character vector
    of the database entries.
    \nReturned value: A character vector, the same length as the vector of IDs,
    containing the paths to the cache files corresponding to the requested entry
    IDs.
    "

    c <- .self$getBiodb()$getPersistentCache()
    fp <- c$getFilePath(.self$getCacheId(), entry.id, .self$getEntryFileExt())

    return(fp)
},

getEntryContent=function(id) {
    ":\n\nGet the contents of database entries from IDs (accession numbers).
    \nid: A character vector of entry IDs.
    \nReturned values: A character vector containing the contents of the
    requested IDs. If no content is available for an entry ID, then NA will be
    used.
    "

    content <- list()
    cch <- .self$getBiodb()$getPersistentCache()
    nm <- .self$getPropertyValue('name')

    if ( ! is.null(id) && length(id) > 0) {

        id <- as.character(id)

        # Debug
        logDebug0("Get ", nm, " entry content(s) for ", length(id),
            " id(s)...")

        # Download full database
        if (.self$isDownloadable())
            .self$download()

        # Initialize content
        if (cch$isReadable(.self) && ! is.null(.self$getCacheId())) {
            # Load content from cache
            content <- cch$loadFileContent(.self$getCacheId(),
                name=id, ext=.self$getEntryFileExt())
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
        if (cch$isReadable(.self)) {
            nld <- sum( ! is.na(id)) - length(missing.ids)
            logDebug("%d %s entry content(s) loaded from cache.", nld, nm)
            if (n.duplicates > 0)
                logDebug0(n.duplicates, " ", nm, " entry ids, whose content",
                    " needs to be fetched, are duplicates.")
        }

        # Get contents
        if (length(missing.ids) > 0
            && ( ! .self$isDownloadable() || ! .self$isDownloaded())) {

            logDebug0(length(missing.ids), " entry content(s) need to be ",
                "fetched from ", nm, " database \"",
                .self$getPropValSlot('urls', 'base.url'), "\".")

            # Divide list of missing ids in chunks
            # (in order to save in cache regularly)
            cs <- .self$getBiodb()$getConfig()$get('dwnld.chunk.size')
            logDebug('dwnld.chunk.size=%d', cs)
            chunks.of.missing.ids <- if (is.na(cs)) list(missing.ids)
                else split(missing.ids, ceiling(seq_along(missing.ids) / cs))
            logDebug('%d chunk(s) to download.', length(chunks.of.missing.ids))

            # Loop on chunks
            missing.contents <- NULL
            for (ch.missing.ids in chunks.of.missing.ids) {

                # Get contents of missing entries
                ec <- .self$getEntryContentFromDb(ch.missing.ids)

                # Save to cache
                if ( ! is.null(ec)
                    && ! is.null(.self$getCacheId()) && cch$isWritable(.self))
                    cch$saveContentToFile(ec,
                        cache.id=.self$getCacheId(), name=ch.missing.ids,
                        ext=.self$getEntryFileExt())

                # Append
                missing.contents <- c(missing.contents, ec)

                # Debug
                if (cch$isReadable(.self)) {
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

getEntryContentFromDb=function(entry.id) {
    ":\n\nGet the contents of entries directly from the database. A direct
    request or an access to the database will be made in order to retrieve the
    contents. No access to the biodb cache system will be made.
    \nentry.id: A character vector with the IDs of entries to retrieve.
    \nReturned value: A character vector, the same size of entry.id, with
    contents of the requested entries. An NA value will be set for the content
    of each entry for which the retrieval failed.
    "

    if (.self$isRemotedb())
        return(.self$.doGetEntryContentOneByOne(entry.id))

    .self$.abstractMethod()
},

getEntryContentRequest=function(entry.id, concatenate=TRUE, max.length=0) {
    ":\n\nGets the URL to use in order to get the contents of the specified
    entries.
    \nentry.id: A character vector with the IDs of entries to retrieve.
    \nconcatenate: If set to TRUE, then try to build as few URLs as
possible, sending requests with several identifiers at once.
    \nmax.length: The maximum length of the URLs to return, in
    number of characters.
    \nReturned value: A list of BiodbUrl objects.
    "

    .self$.checkIsRemote()
    urls <- character(0)

    if (length(entry.id) > 0) {

        # Get full URL
        full.url <- .self$.doGetEntryContentRequest(entry.id,
            concatenate=concatenate)

        # No single URL for multiple IDs
        if ((length(entry.id) > 1 && length(full.url) > 1) || max.length == 0
            || nchar(full.url) <= max.length)
            urls <- full.url

        # full.url is too big, we must split it
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
                    url <- .self$.doGetEntryContentRequest(entry.id[start:m])
                    if (all(nchar(url) <= max.length) && m != a)
                        a <- m
                    else
                        b <- m
                }
                urls <- c(urls,
                    .self$.doGetEntryContentRequest(entry.id[start:a]))
                start <- a + 1
            }
        }
    }

    return(urls)
},

getEntryIds=function(max.results=0, ...) {
    ":\n\nGet entry identifiers from the database. More arguments can be given,
    depending on implementation in specific databases. For mass databases, the
    ones derived from BiodbBiodbMassdbConn class, the ms.level argument can be
    set.
    \nmax.results: The maximum of elements to return from the method.
    \n...: First arguments to be passed to private .doGetEntryIds() method.
    \nReturned value: A character vector containing entry IDs from the
    database. An empty vector for a remote database may mean that the database
    does not support requesting for entry accessions.
    "

    chk::chk_number(max.results)
    chk::chk_gte(max.results, 0)

    ids <- character()

    # Get IDs from volatile cache
    not.null <- ! vapply(.self$.entries, is.null, FUN.VALUE=T)
    ids <- names(.self$.entries[not.null])

    # Get IDs from database
    if (max.results == 0 || length(ids) < max.results) {
        db.ids <- .self$.doGetEntryIds(max.results=max.results, ...)
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

getNbEntries=function(count=FALSE) {
    ":\n\nGet the number of entries contained in this database.
    \ncount: If set to TRUE and no straightforward way exists to get number of
    entries, count the output of getEntryIds().
    \nReturned value: The number of entries in the database, as an integer.
    "

    n <- NA_integer_

    if (count) {
        ids <- .self$getEntryIds()
        if ( ! is.null(ids))
            n <- length(ids)
    }

    return(n)
},

isEditable=function() {
    ":\n\nTests if this connector is able to edit the database (i.e.: the
    connector class implements the interface BiodbEditable). If this connector
    is editable, then you can call allowEditing() to enable editing.
    \nReturned value: Returns TRUE if the database is editable.
    "

    return(.self$getPropertyValue('editable'))
},

.checkIsEditable=function() {
    if ( ! .self$isEditable())
        error0("The database associated to this connector ", .self$getId(),
            " is not editable.")
},

editingIsAllowed=function() {
    ":\n\nTests if editing is allowed.
    \nReturned value: TRUE if editing is allowed for this database, FALSE
    otherwise.
    "
    
    .self$.checkIsEditable()
    .self$.initEditable()

    return(.self$.editing.allowed)
},

allowEditing=function() {
    ":\n\nAllows editing for this database.
    \nReturned value: None.
    "

    .self$.checkIsEditable()
    .self$setEditingAllowed(TRUE)
},

disallowEditing=function() {
    ":\n\nDisallows editing for this database.
    \nReturned value: None.
    "
    
    .self$.checkIsEditable()
    .self$setEditingAllowed(FALSE)
},

setEditingAllowed=function(allow) {
    ":\n\nAllow or disallow editing for this database.
    \nallow: A logical value.
    \nReturned value: None.
    "
    
    chk::chk_logical(allow)

    .self$.checkIsEditable()
    .self$.editing.allowed <- allow
},

addNewEntry=function(entry) {
    ":\n\nAdds a new entry to the database. The passed entry must have been
    previously created from scratch using BiodbFactory::createNewEntry() or
    cloned from an existing entry using BiodbEntry::clone().
    \nentry: The new entry to add. It must be a valid BiodbEntry object.
    \nReturned value: None.
    "

    .self$.checkIsEditable()
    .self$.checkEditingIsAllowed()

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
    e <- .self$getEntry(id)
    if ( ! is.null(e))
        error0('Impossible to add entry as a new entry. The accession',
            ' number of the passed entry is already used in the',
            ' connector.')

    # Make sure ID field is equal to accession
    id.field <- .self$getEntryIdField()
    if ( ! entry$hasField(id.field) || entry$getFieldValue(id.field) != id)
        entry$setFieldValue(id.field, id)

    # Remove entry from non-volatile cache
    cch <- .self$getBiodb()$getPersistentCache()
    if (cch$isWritable(.self))
        cch$deleteFile(.self$getCacheId(), name=id, ext=.self$getEntryFileExt())

    # Flag entry as new
    entry$.setAsNew(TRUE)

    # Set the connector as its parent
    entry$.setParent(.self)

    # Add entry to volatile cache
    .self$.addEntriesToCache(id, list(entry))
},

.initEditable=function() {
    if (length(.self$.editing.allowed) == 0)
        .self$setEditingAllowed(FALSE)
},

.checkEditingIsAllowed=function() {

    .self$.initEditable()

    if ( ! .self$.editing.allowed)
        error0('Editing is not enabled for this database. However this',
            ' database type is editable. Please call allowEditing()',
            ' method to enable editing.')
},

isWritable=function() {
    ":\n\nTests if this connector is able to write into the database.  If this
    connector is writable, then you can call allowWriting() to enable writing.
    \nReturned value: Returns TRUE if the database is writable.
    "

    return(.self$getPropertyValue('writable'))
},

.checkIsWritable=function() {
    if ( ! .self$isWritable())
        error0("The database associated to this connector ", .self$getId(),
            " is not writable.")
},

allowWriting=function() {
    ":\n\nAllows the connector to write into this database.
    \nReturned value: None.
    "

    .self$.checkIsWritable()
    .self$setWritingAllowed(TRUE)
},

disallowWriting=function() {
    ":\n\nDisallows the connector to write into this database.
    \nReturned value: None.
    "
    
    .self$.checkIsWritable()
    .self$setWritingAllowed(FALSE)
},

setWritingAllowed=function(allow) {
    ":\n\nAllows or disallows writing for this database.
    \nallow: If set to TRUE, allows writing.
    \nReturned value: None.
    "
    
    .self$.checkIsWritable()
    chk::chk_logical(allow)
    .self$.writing.allowed <- allow
},

writingIsAllowed=function() {
    ":\n\nTests if the connector has access right to the database.
    \nReturned value: TRUE if writing is allowed for this database, FALSE
    otherwise.
    "
    
    .self$.checkIsWritable()
    .self$.initWritable()

    return(.self$.writing.allowed)
},

write=function() {
    ":\n\nWrites into the database. All modifications made to the database since
    the last time write() was called will be saved.
    \nReturned value: None.
    "

    .self$.checkIsWritable()
    .self$.checkWritingIsAllowed()
    .self$.doWrite()

    # Unset "new" flag for all entries
    for (e in .self$.entries)
        e$.setAsNew(FALSE)
},

.checkWritingIsAllowed=function() {
    
    .self$.initWritable()
    
    if ( ! .self$.writing.allowed)
        error0('Writing is not enabled for this database. However this',
            ' database type is writable. Please call allowWriting()',
            ' method to enable writing.')
},

.doWrite=function() {
    .self$.abstractMethod()
},

.initWritable=function() {
    if (length(.self$.writing.allowed) == 0)
        .self$setWritingAllowed(FALSE)
},

isSearchableByField=function(field) {
    ":\n\nTests if a field can be used to search entries when using methods
    searchByName() and searchCompound().
    \nfield: The name of the field.
    \nReturned value: Returns TRUE if the database is searchable using the
    specified field, FALSE otherwise.
    "

    v <- FALSE

    ef <- .self$getBiodb()$getEntryFields()
    field <- ef$getRealName(field)
    for (sf in .self$getPropertyValue('searchable.fields'))
        if (ef$getRealName(sf) == field) {
            v <- TRUE
            break;
        }

    return(v)
},

getSearchableFields=function() {
    ":\n\nGet the list of all searchable fields.
    \nReturned value: A character vector containing all searchable fields for
    this connector.
    "
    
    # Get all searchable candidates
    fields <- .self$getPropertyValue('searchable.fields')
    
    # Filter out those that are not searchable with this instance
    # This is for dynamic connectors like CsvFileConn whose list of available
    # fields may vary.
    fields <- Filter(function(f) .self$isSearchableByField(f), fields)
    
    return(fields)
},

searchForEntries=function(fields=NULL, max.results=0) {
    ":\n\nSearches the database for entries whose name matches the specified
    name.  Returns a character vector of entry IDs.
    \nfields: A list of fields on which to filter entries. To get a match, all
    fields must be matched (i.e.: logical AND). The keys of the list are the
    entry field names on which to filter, and the values are the filtering
    parameters. For character fields, the filter parameter is a character
    vector in which all strings must be found inside the field's value. For
    numeric fields, the filter parameter is either a list specifying a min-max
    range (`list(min=1.0, max=2.5)`) or a value with a tolerance in delta
    (`list(value=2.0, delta=0.1)`) or ppm (`list(value=2.0, ppm=1.0)`).
    \nmax.results: If set, the number of returned IDs is limited to this
    number.
    \nReturned value: A character vector of entry IDs whose name matches the
    requested name.
    "

    chk::chk_null_or(fields, chk::chk_list)
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
        else if ( ! .self$isSearchableByField(f)) {
            warn('This database is not searchable by field "%s".', f)
            fields[[f]] <- NULL
            wrong_fields <- TRUE
        }
    }
        
    # Call concrete method
    if (length(fields) > 0 || ! wrong_fields)
        ids <- .self$.doSearchForEntries(fields=fields, max.results=max.results)

    # Convert NULL to empty list
    if (is.null(ids))
        ids <- character()

    # Cut
    if (max.results > 0 && max.results < length(ids))
        ids <- ids[seq_len(max.results)]

    return(ids)
},

.doSearchForEntries=function(fields=NULL, max.results=0) {
    # To be implemented by derived class.
    return(NULL)
},

searchByName=function(name, max.results=0) { # DEPRECATED
    ":\n\nThis method is deprecated.
    \nUse searchForEntries() instead.
    "
    
    lifecycle::deprecate_warn('1.0.0', 'searchByName()', "searchForEntries()")
    ids <- NULL

    # Try deprecated method searchCompound()
    if (.self$isCompounddb())
        ids <- .self$searchCompound(name=name, max.results=max.results)
    else
        ids <- .self$searchForEntries(list(name=name), max.results=max.results)

    return(ids)
},

isDownloadable=function() {
    ":\n\nTests if the connector can download the database.
    \nReturned value: Returns TRUE if the database is downloadable.
    "

    return(.self$getPropertyValue('downloadable'))
},

.checkIsDownloadable=function() {
    if ( ! .self$isDownloadable())
        error0("The database associated to this connector ", .self$getId(),
            " is not downloadable.")
},

isDownloaded=function() {
    ":\n\nTests if the database has been downloaded.
    \nReturned value: TRUE if the database content has already been downloaded.
    "

    .self$.checkIsDownloadable()
    cch <- .self$getBiodb()$getPersistentCache()
    dwnlded  <- cch$markerExist(.self$getCacheId(),
                    name='downloaded')

    s <- (if (dwnlded) 'already' else 'not yet')
    logDebug0('Database ', .self$getId(), ' has ', s, ' been downloaded.')

    return(dwnlded)
},

requiresDownload=function() {
    return(FALSE)
},

getDownloadPath=function() {
    ":\n\nGets the path where the downloaded content is written.
    \nReturned value: The path where the downloaded database is written.
    "

    .self$.checkIsDownloadable()
    cch <- .self$getBiodb()$getPersistentCache()
    ext <- .self$getPropertyValue('dwnld.ext')
    path <- cch$getFilePath(.self$getCacheId(), name='download', ext=ext)

    logDebug0('Download path of ', .self$getId(), ' is "', path, '".')

    return(path)
},

isExtracted=function() {
    ":\n\nTests if the downloaded database has been extracted (in case the
    database needs extraction).
    \nReturned value: TRUE if the downloaded database content has been
    extracted, FALSE otherwise.
    "

    .self$.checkIsDownloadable()
    cch <- .self$getBiodb()$getPersistentCache()
    return(cch$markerExist(.self$getCacheId(),
        name='extracted'))
},

download=function() {
    ":\n\nDownloads the database content locally.
    \nReturned value: None.
    "

    .self$.checkIsDownloadable()
    cch <- .self$getBiodb()$getPersistentCache()

    # Download
    cfg <- .self$getBiodb()$getConfig()
    if (cch$isWritable(.self) && ! .self$isDownloaded()
        && (cfg$isEnabled('allow.huge.downloads') || .self$requiresDownload())
        && ! cfg$isEnabled('offline')) {

        logInfo0("Downloading whole database of ", .self$getId(), ".")
        .self$.doDownload()
        if ( ! file.exists(.self$getDownloadPath()))
            error("File %s does not exists. Downloading went wrong.",
                .self$getDownloadPath())
        logDebug0('Downloading of ', .self$getId(), ' completed.')

        # Set marker
        cch$setMarker(.self$getCacheId(), name='downloaded')
    }

    # Extract
    if (.self$isDownloaded() && ! .self$isExtracted()) {

        logInfo0("Extract whole database of ", .self$getId(), ".")

        .self$.doExtractDownload()

        # Set marker
        cch$setMarker(.self$getCacheId(), name='extracted')
    }
},

.doDownload=function() {
    .self$.abstractMethod()
},

.doExtractDownload=function() {
    .self$.abstractMethod()
},

.checkIsRemote=function() {
    if ( ! .self$isRemotedb())
        error0("The database associated to this connector ", .self$getId(),
            " is not a remote database.")
},

isRemotedb=function() {
    ":\n\nTests if the connector is connected to a remote database.
    \nReturned value: Returns TRUE if the database is a remote database."

    return(.self$getPropertyValue('remote'))
},

isCompounddb=function() {
    ":\n\nTests if the connector's database is a compound database (i.e.: the
    connector class inherits from BiodbCompounddbConn class).
    \nReturned value: Returns TRUE if the database is a compound database.
    "

    return(methods::is(.self, 'BiodbCompounddbConn'))
},

isMassdb=function() {
    ":\n\nTests if the connector's database is a mass spectra database (i.e.:
    the connector class inherits from BiodbMassdbConn class).
    \nReturned value: Returns TRUE if the database is a mass database.
    "

    return(methods::is(.self, 'BiodbMassdbConn'))
},

checkDb=function() {
    ":\n\nChecks that the database is correct by trying to retrieve all its
    entries.
    \nReturned values: None."

    # Get IDs
    ids <- .self$getEntryIds()

    # Get entries
    entries <- .self$getBiodb()$getFactory()$getEntry(.self$getId(), ids)
},

getAllVolatileCacheEntries=function() {
    ":\n\nGet all entries stored in the memory cache (volatile cache).
    \nReturned value: A list of BiodbEntry instances.
    "

    # Remove NULL entries
    entries <- .self$.entries[ ! vapply(.self$.entries, is.null,
                                        FUN.VALUE=TRUE)]

    # Remove names
    names(entries) <- NULL

    return(entries)
},

getAllCacheEntries=function() { # DEPRECATED
    ":\n\nThis method is deprecated.
    \nUse getAllVolatileCacheEntries() instead.
    "
    lifecycle::deprecate_soft('1.0.0', 'getAllCacheEntries()',
        "getAllVolatileCacheEntries()")
    .self$getAllVolatileCacheEntries()
},

deleteAllEntriesFromVolatileCache=function() {
    ":\n\nDelete all entries from the volatile cache (memory cache).
    \nReturned value: None.
    "

    .self$.entries <- list()
    
    return(invisible(NULL))
},

deleteAllEntriesFromPersistentCache=function(deleteVolatile=TRUE) {
    ":\n\nDelete all entries from the persistent cache (disk cache).
    \ndeleteVolatile: If TRUE deletes also all entries from the volatile cache
    (memory cache).
    \nReturned value: None.
    "

    if (deleteVolatile)
        .self$deleteAllEntriesFromVolatileCache()
    fileExt <- .self$getPropertyValue('entry.content.type')
    .self$getBiodb()$getPersistentCache()$deleteFiles(.self$getCacheId(),
        ext=fileExt)
    
    return(invisible(NULL))
},

deleteWholePersistentCache=function(deleteVolatile=TRUE) {
    ":\n\nDelete all files associated with this connector from the persistent
    cache (disk cache).  \ndeleteVolatile: If TRUE deletes also all entries
    from the volatile cache (memory cache).
    \nReturned value: None.
    "

    if (deleteVolatile)
        .self$deleteAllEntriesFromVolatileCache()
    .self$getBiodb()$getPersistentCache()$deleteAllFiles(.self$getCacheId())
},

deleteAllCacheEntries=function() { # DEPRECATED
    ":\n\nDelete all entries from the memory cache. This method is deprecated,
    please use deleteAllEntriesFromVolatileCache() instead.
    \nReturned value: None.
    "
    lifecycle::deprecate_soft('1.0.0', 'deleteAllCacheEntries()',
        "deleteAllEntriesFromVolatileCache()")
    .self$deleteAllEntriesFromVolatileCache()
},

getCacheId=function() {
    ":\n\nGets the ID used by this connector in the disk cache.
    \nReturned value: The cache ID of this connector.
    "

    id <- NULL

    if ( ! is.null(.self$.cache.id) && ! is.na(.self$.cache.id)) {
        id <- .self$.cache.id

    } else {
        url <- .self$getPropValSlot('urls', 'base.url')
        if ( ! is.null(url) && ! is.na(url))
            id <- paste(.self$getDbClass(), openssl::md5(url), sep='-')
    }

    return(id)
},

makesRefToEntry=function(id, db, oid, any=FALSE, recurse=FALSE) {
    ":\n\nTests if some entry of this database makes reference to another entry
    of another database.
    \nid: A character vector of entry IDs from the connector's database.
    \ndb: Another database connector.
    \noid: A entry ID from database db.
    \nany: If set to TRUE, returns a single logical value: TRUE if any entry
    contains a reference to oid, FALSE otherwise.
    \nrecurse: If set to TRUE, the algorithm will follow all references to
    entries from other databases, to see if it can establish an indirect link
    to `oid`.
    \nReturned value: A logical vector, the same size as `id`, with TRUE for
    each entry making reference to `oid`, and FALSE otherwise.
    "

    # Returns TRUE if any entry in id makes reference to oid
    if (any) {
        makes_ref <- FALSE
        for (i in id) {
            e <- .self$getEntry(i)
            if ( ! is.null(e)
                && e$makesRefToEntry(db=db, oid=oid, recurse=recurse)) {
                makes_ref <- TRUE
                break
            }
        }
    }

    # Returns a vector, testing each entry in id individually
    else {
        entries <- .self$getEntry(id, drop=FALSE)
        makes_ref <- vapply(entries,
            function(e) ! is.null(e) && e$makesRefToEntry(db=db, oid=oid,
            recurse=recurse), FUN.VALUE=TRUE)
    }
    return(makes_ref)
},

makeRequest=function(...) {
    ":\n\nMakes a BiodbRequest instance using the passed parameters, and set
    ifself as the associated connector.
    \n...: Those parameters are passed to the initializer of BiodbRequest.
    \nReturned value: The BiodbRequest instance.
    "

    req <- BiodbRequest$new(...)

    req$setConn(.self)

    return(req)
},

getEntryImageUrl=function(entry.id) {
    ":\n\nGets the URL to a picture of the entry (e.g.: a picture of the
    molecule in case of a compound entry).
    \nentry.id: A character vector containing entry IDs.
    \nReturned value: A character vector, the same length as `entry.id`,
    containing for each entry ID either a URL or NA if no URL exists.
    "

    .self$.checkIsRemote()
    return(rep(NA_character_, length(entry.id)))
},

getEntryPageUrl=function(entry.id) {
    ":\n\nGets the URL to the page of the entry on the database web site.
    \nentry.id: A character vector with the IDs of entries to retrieve.
    \nReturned value: A list of BiodbUrl objects, the same length as `entry.id`.
    "

    .self$.checkIsRemote()
    .self$.abstractMethod()
},

.doGetEntryContentRequest=function(id, concatenate=TRUE) {
    .self$.checkIsRemote()
    .self$.abstractMethod()
},

.doGetEntryContentOneByOne=function(entry.id) {

    .self$.checkIsRemote()

    # Initialize return values
    content <- rep(NA_character_, length(entry.id))

    # Get requests
    requests <- .self$getEntryContentRequest(entry.id, concatenate=FALSE)

    # Get encoding
    encoding <- .self$getPropertyValue('entry.content.encoding')

    # If requests is a vector of characters, then the method is using the old
    # scheme.
    # We now convert the requests to the new scheme, using class BiodbRequest.
    if (is.character(requests)) {
        fct <- function(x) .self$makeRequest(method='get', url=BiodbUrl$new(x),
            encoding=encoding)
        requests <- lapply(requests, fct)
    }

    # Send requests
    scheduler <- .self$getBiodb()$getRequestScheduler()
    prg <- Progress$new(biodb=.self$getBiodb(),
                        msg='Downloading entry contents',
                        total=length(requests))
    for (i in seq_along(requests)) {
        prg$increment()
        content[[i]] <- scheduler$sendRequest(requests[[i]])
    }

    return(content)
},

.doGetEntryIds=function(max.results=0) {
    .self$.abstractMethod()
},

.addEntriesToCache=function(ids, entries) {

    ids <- as.character(ids)

    names(entries) <- ids

    # Update known entries
    known.ids <- ids[ids %in% names(.self$.entries)] 
    .self$.entries[known.ids] <- entries[ids %in% known.ids]

    # Add new entries
    new.ids <- ids[ ! ids %in% names(.self$.entries)]
    .self$.entries <- c(.self$.entries, entries[ids %in% new.ids])
},

.getEntriesFromCache=function(ids) {

    ids <- as.character(ids)

    return(.self$.entries[ids])
},

.getEntryMissingFromCache=function(ids) {

    ids <- as.character(ids)

    missing.ids <- ids[ ! ids %in% names(.self$.entries)]

    return(missing.ids)
},

.terminate=function() {

    # Unregister from the request scheduler
    if (.self$isRemotedb())
        .self$getBiodb()$getRequestScheduler()$.unregisterConnector(.self)

    callSuper()
}

))
