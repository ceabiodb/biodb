#' A class for accessing the test reference entries.
#'
#' The test reference entries are stored as JSON files inside
#' `inst/testref` folder of each extension package.
#'
#' @examples
#' # Creates an instance
#' refEntries <- TestRefEntries$new('comp.sqlite', pkgName='biodb')
#'
#' # Gets identifiers of all reference entries
#' refEntries$getAllIds()
#'
#' # Gets a data frame with the content of the reference entries
#' refEntries$getAllRefEntriesDf()
#'
#' @import R6
#' @import plyr
#' @export
TestRefEntries <- R6::R6Class('TestRefEntries',

public=list(

#' @description
#' New instance initializer.
#' @param db.class Identifier of the database.
#' @param pkgName Name of the package in which are stored the reference
#' entry files.
#' @param folder The folder where to find test reference files for the package.
#' Usually it is "inst/testref".
#' @param bdb A valid BiodbMain instance or NULL.
#' @return Nothing.
initialize=function(db.class, pkgName, folder=NULL, bdb=NULL) {
    chk::chk_string(db.class)
    chk::chk_string(pkgName)
    chk::chk_null_or(bdb, vld=chk::vld_is, class='BiodbMain')
    chk::chk_null_or(folder, vld=chk::vld_dir)
    private$db.class <- db.class
    private$pkgName <- pkgName
    private$bdb <- bdb
    private$folder <- folder
    return(invisible(NULL))
}

#' @description
#' Retrieve all identifiers.
#' @param limit The maximum number of identifiers to return.
#' @return A character vector containing the IDs.
,getAllIds=function(limit=0) {
    chk::chk_whole_number(limit)
    chk::chk_gte(limit, 0)

    # List JSON files
    files <-  private$getRefEntryJsonFiles()

    # Remove content files
    files <- grep('-content.json$', files, value=TRUE, invert=TRUE)

    # Limit number of files
    if (limit > 0 && length(files) > limit)
        files <- files[seq_len(limit)]

    # Extract ids
    ids <- sub(paste('^.*/entry', private$db.class, '(.+)\\.json$', sep='-'),
        '\\1', files, perl=TRUE)

    # Replace encoded special characters
    ids = vapply(ids, utils::URLdecode, FUN.VALUE='')

    return(ids)
}

#' @description
#' Get the reference contents for the specfied IDs.
#' @param ids The reference IDs for which to get the contents.
#' @return A character vector.
,getContents=function(ids) {

    chk::chk_character(ids)

    # Get file names
    testRef <- private$getFolder()
    getFilename <- function(id) {
        pattern <- file.path(testRef, paste0('entry-', private$db.class, '-',
            utils::URLencode(id, reserved=TRUE), '-content.*'))
        files <- Sys.glob(pattern)
        if (length(files) == 0) {
            # Entry content file is missing in test ref dir.
            warn(paste("Content file is missing for test ref entry %s.",
                "A content will now be retrieved from the connector",
                "and saved into test ref folder %s"), id, testRef)
            files <- private$downloadEntryContent(id)
        } else if (length(files) > 1)
            error("Found more than one file for pattern %s: %s.", pattern,
                paste(files, collapse=', '))
        return(files) # Got one single file
    }
    filenames <- vapply(ids, getFilename, FUN.VALUE='')

    # Load contents
    contents <- loadFileContents(filenames, outVect=TRUE)

    return(contents)
}

#' @description
#' Retrieve all real entries from database corresponding to the reference
#' entris.
#' @param ids A character vector of entry identifiers.
#' @return A list containing BiodbEntry instances.
,getRealEntries=function(ids=NULL) {
    chk::chk_null_or(ids, vld=chk::vld_character)
    if (is.null(private$bdb))
        error(paste("A valid BiodbMain instance is needed inside the",
            "TestRefEntries instance to retrieve real entries."))
    ref.ids <- self$getAllIds()
    if (is.null(ids))
        ids <- ref.ids
    else
        chk::chk_subset(ids, ref.ids)

    # Create entries
    entries <- private$bdb$getFactory()$getEntry(private$db.class, id=ids,
        drop=FALSE)
    testthat::expect_equal(length(entries), length(ids),
        info=paste0("Error while retrieving entries. ", length(entries),
        " entrie(s) obtained instead of ", length(ids),
        "."))
    testthat::expect_false(any(vapply(entries, is.null, FUN.VALUE=TRUE)))

    # Compute fields
    private$bdb$computeFields(entries)

    # Save downloaded entries as JSON
    self$saveEntriesAsJson(ids, entries)

    return(entries)
}

#' @description
#' Saves a list of entries into separate JSON files, inside the test output
#' folder.
#' @param ids The IDs of the entries.
#' @param entries A list of entries. It can contain NULL values.
#' @return Nothing.
,saveEntriesAsJson=function(ids, entries) {

    filenames <- paste('entry-', private$db.class, '-', ids, '.json',
        sep='')
    filenames <- vapply(filenames, utils::URLencode, FUN.VALUE='',
        reserved=TRUE)
    json.files <- file.path(getTestOutputDir(), filenames)
    private$bdb$saveEntriesAsJson(entries, json.files)
}

#' @description
#' Retrieves one real entry from database corresponding to one reference
#' entry.
#' @param id The identifier of the entry.
#' @return A BiodbEntry instance.
,getRealEntry=function(id) {
    return(self$getRealEntries(ids=id)[[1]])
}

#' @description
#' Retrieves the content of a single reference entry.
#' @param id The identifier of the reference entry to retrieve.
#' @return The content of the reference entry as a list.
,getRefEntry=function(id) {
    chk::chk_string(id)

    # Replace forbidden characters
    id = utils::URLencode(id, reserved=TRUE)

    # Entry file
    file <- file.path(private$getFolder(),
        paste('entry-', private$db.class, '-', id, '.json', sep=''))
    testthat::expect_true(file.exists(file),
        info=paste0('Cannot find file "', file, '" for ', private$db.class,
        ' reference entry', id, '.'))

    # Load JSON
    json <- jsonlite::fromJSON(file)

    # Set NA values
    for (n in names(json))
        if (length(json[[n]]) == 1) {
            if (json[[n]] == 'NA_character_')
                json[[n]] <- NA_character_
        }

    return(json)
}

#' @description
#' Load all reference entries.
#' @return A data frame containing all the reference entries with their values.
,getAllRefEntriesDf=function() {

    entries.desc <- NULL

    # List JSON files
    entry.json.files <- private$getRefEntryJsonFiles()

    # Loop on all JSON files
    for (f in entry.json.files) {

        # Load entry from JSON
        entry <- jsonlite::read_json(f)

        # Replace NULL values by NA
        entry <- lapply(entry, function(x) if (is.null(x)) NA else x)

        # Convert to data frame
        entry.df <- as.data.frame(entry, stringsAsFactors = FALSE)

        # Append entry to main data frame
        entries.desc <- plyr::rbind.fill(entries.desc, entry.df)
    }

    return(entries.desc)
}
),
private=list(
    db.class=NULL,
    pkgName=NULL,
    bdb=NULL,
    folder=NULL

,getFolder=function() {

    if (is.null(private$folder)) {
        private$folder <- system.file('testref', package=private$pkgName)
        if ( ! dir.exists(private$folder))
            error('No folder "%s" has been defined for package "%s".',
                private$folder, private$pkgName)
    }

    return(private$folder)
}

,getRefEntryJsonFiles=function() {

    # List JSON files
    files <- Sys.glob(file.path(private$getFolder(),
        paste('entry', private$db.class, '*.json', sep='-')))

    # Remove content files
    files <- grep('-content.json$', files, value=TRUE, invert=TRUE)

    return(files)
}

,downloadEntryContent=function(id) {

    testRef <- private$getFolder()
    if (is.null(private$bdb))
        error(paste("A valid BiodbMain instance is needed inside the",
            "TestRefEntries instance to retrieve the content of entry %s and",
            "save it inside the test ref folder %s."), id, testRef)

    # Get content
    conn <- private$bdb$getFactory()$getConn(private$db.class)
    content <- conn$getEntryContent(id)
    if (is.null(content))
        error("Unable to retrieve content of entry %s from connector %s.", id,
            private$db.class)

    # Save content to file
    ext <- conn$getEntryFileExt()
    f <- paste0('entry-', private$db.class, '-', id, '-content.', ext)
    f <- utils::URLencode(f, reserved=TRUE)
    f <- file.path(testRef, f)
    saveContentsToFiles(f, content, prepareContents=TRUE)

    return(f)
}
))
