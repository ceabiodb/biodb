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
#' @export
TestRefEntries <- R6::R6Class('TestRefEntries',

public=list(

#' @description
#' New instance initializer.
#' @param db.class Identifier of the database.
#' @param pkgName Name of the package in which are stored the reference
#' entry files.
#' @return Nothing.
initialize=function(db.class, pkgName=NULL) {
    chk::chk_string(db.class)
    chk::chk_null_or(db.class, chk::chk_string)
    private$db.class <- db.class
    private$pkgName <- pkgName
    return(invisible(NULL))
}

#' @description
#' Retrieve all identifiers.
#' @param limit The maximum number of identifiers to return.
#' @return A character vector containing the IDs.
,getAllIds=function(limit=0) {
    chk::chk_whole_number(limit)
    chk::chk_gte(limit, 0)

    # Get test ref folder
    testRef <- private$getFolder()

    # List JSON files
    files <- Sys.glob(file.path(testRef, paste('entry', private$db.class,
        '*.json', sep='-')))
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
#' Retrieve all real entries from database corresponding to the reference
#' entries.
#' @param bdb A valid BiodbMain instance.
#' @return A list containing Entry instances.
,getRealEntries=function(bdb) {
    chk::chk_is(bdb, 'BiodbMain')

    ref.ids <- self$getAllIds()

    # Create entries
    entries <- bdb$getFactory()$getEntry(private$db.class, id=ref.ids,
        drop=FALSE)
    testthat::expect_equal(length(entries), length(ref.ids),
        info=paste0("Error while retrieving entries. ", length(entries),
        " entrie(s) obtained instead of ", length(ref.ids),
        "."))
    testthat::expect_false(any(vapply(entries, is.null, FUN.VALUE=TRUE)))

    # Compute fields
    bdb$computeFields(entries)

    # Save downloaded entries as JSON
    filenames <- paste('entry-', private$db.class, '-', ref.ids, '.json',
        sep='')
    filenames <- vapply(filenames, utils::URLencode, FUN.VALUE='',
        reserved=TRUE)
    json.files <- file.path(getTestOutputDir(), filenames)
    bdb$saveEntriesAsJson(entries, json.files)

    return(entries)
}

#' @description
#' Retrieves the content of a single reference entry.
#' @param id The identifier of the reference entry to retrieve.
#' @return The content of the reference entry as a list.
,getRefEntry=function(id) {

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
    entry.json.files <- Sys.glob(file.path(private$getFolder(),
        paste('entry', private$db.class, '*.json', sep='-')))

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
    pkgName=NULL

,getFolder=function() {

    testRef <- NULL

    if ( ! is.null(private$pkgName)) {
        testRef <- system.file('testref', package=private$pkgName)
        if ( ! dir.exists(testRef))
            error("No folder %s has been defined for package %s.", testRef,
                private$pkgName)
    }
    else {
        # Look for testref folder in ../../inst or in ../../<pkg_name>
        # (<pkg>.Rcheck folder)
        testRef <- Sys.glob(file.path(getwd(), '..', '..', '*', 'testref'))[[1]]
        
        # No folder
        if ( ! dir.exists(testRef)) {

            oldTestRef <- file.path(getwd(), '..', 'testthat', 'res')
            if (dir.exists(oldTestRef))
                warn0("The location of reference entry files for tests has",
                    ' changed. Please move folder "', oldTestRef, '" to "', 
                    testRef, '".')
            error("No folder %s has been defined.", testRef)
        }
    }

    return(testRef)
}
))
