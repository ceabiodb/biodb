#' {{dbTitle}} connector class.
#'
#' Connector class for {{dbTitle}}.
#'
#' @seealso \code{\link{BiodbConn}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Get a connector:
#' conn <- mybiodb$getFactory()$createConn('{{dbName}}')
#'
#' # Get the first entry
#' e <- conn$getEntry(conn$getEntryIds(1L))
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @import biodb
#' @import R6
#' @export
{{connClass}} <- R6::R6Class("{{connClass}}",
inherit=biodb::BiodbConn,

public=list(

initialize=function(...) {
    super$initialize(...)
}

,getNbEntries=function(count=FALSE) {
    # Overrides super class' method.

    # Replace the call below if you have a direct way (specific web service for
    # a remote database, provided method or information for a local database)
    # to count entries for your database.
    return(callSuper(count=count))
}

,getEntryContentFromDb=function(entry.id) {
    # Overrides super class' method.

    # Initialize return values
    content <- rep(NA_character_, length(entry.id))

    # TODO Implement retrieval of entry contents.

    # Some debug message
    if (length(content) > 0)
        .self$message('debug', paste("Content of first entry:", content[[1]]))

    return(content)
}
# $$$ CASE CONNTYPE MASS $$$

,getChromCol=function(ids=NULL) {
    # TODO Implement
}

,getNbPeaks=function(mode=NULL, ids=NULL) {
    # TODO Implement
}
# $$$ END_CASE CONNTYPE $$$
# $$$ SECTION REMOTE $$$

,getEntryPageUrl=function(id) {
    # Overrides super class' method.

    # TODO Modify this code to build the individual URLs to the entry web pages
    fct <- function(x) {
        u <- c(.self$getPropValSlot('urls', 'base.url'), 'entries', x)
        BiodbUrl(url=u)$toString()
    }

    return(vapply(id, fct, FUN.VALUE=''))
}
# $$$ END_SECTION REMOTE $$$
# $$$ SECTION REMOTE $$$

,getEntryImageUrl=function(id) {
    # Overrides super class' method.

    # TODO Modify this code to build the individual URLs to the entry images 
    fct <- function(x) {
        u <- c(.self$getPropValSlot('urls', 'base.url'), 'images', x,
               'image.png')
        BiodbUrl(url=u)$toString()
    }

    return(vapply(id, fct, FUN.VALUE=''))
}
# $$$ END_SECTION REMOTE $$$
# $$$ SECTION REMOTE $$$

,wsFind=function(name="", retfmt=c('plain', 'parsed', 'ids', 'request')) {
    # This is the implementation of a fictive web service called "find" that
    # search for entries by name.
    # Use it as an example for implementing your own web services.

    retfmt <- match.arg(retfmt)

    # Build request
    params <- list(name=name)
    url <- BiodbUrl(url=c(.self$getPropValSlot('urls', 'ws.url'), 'find'),
                    params=params)
    request <- .self$makeRequest(method='get', url=url)

    # Return request
    if (retfmt == 'request')
        return(request)

    # Send request
    # This the line that should be run for sending the request and getting the
    # results:
    #results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)
    # Instead, for this example, we just generate the results of this fictive
    # web service:
    results <- paste('{"0001": {"name": "name1"},',
                     ' "0198": {"name": "name2"},',
                     ' "9834": {"name": "name3"}}')

    # Parse
    if (retfmt != 'plain') {
        
        # Parse JSON
        results <- jsonlite::fromJSON(results, simplifyDataFrame=FALSE)

        # Get IDs
        if (retfmt == 'ids')
            results <- names(results)
    }

    return(results)
}
# $$$ END_SECTION REMOTE $$$
),

private=list(

,doGetEntryIds=function(max.results=NA_integer_) {
    # Overrides super class' method.

    ids <- NA_character_
 
    # TODO Implement retrieval of accession numbers.
    
    return(ids)
}

,doSearchForEntries=function(fields=NULL, max.results=NA_integer_) {
    # Overrides super class' method.

    ids <- character()

    # TODO Implement search of entries by filtering on values of fields.
    
    return(ids)
}
# $$$ CASE CONNTYPE COMPOUND $$$
# $$$ CASE CONNTYPE MASS $$$

,doSearchMzRange=function(mz.min, mz.max, min.rel.int, ms.mode, max.results,
                          precursor, ms.level) {
    # TODO Implement
}

,doGetMzValues=function(ms.mode, max.results, precursor, ms.level) {
    # TODO Implement
}
# $$$ END_CASE CONNTYPE $$$
# $$$ SECTION REMOTE $$$

,doGetEntryContentRequest=function(id, concatenate=TRUE) {

    # TODO Modify the code below to build the URLs to get the contents of the
    # entries.
    # Depending on the database, you may have to build one URL for each
    # individual entry or may be able to write just one or a few URL for all
    # entries to retrieve.
    u <- c(.self$getPropValSlot('urls', 'base.url'), 'entries',
           paste(id, 'xml', sep='.'))
    url <- BiodbUrl(url=u)$toString()

    return(url)
}
# $$$ END_SECTION REMOTE $$$
# $$$ SECTION DOWNLOADABLE $$$

,doDownload=function() {

    biodb::logInfo("Downloading {{dbTitle}}...")

    # TODO Build the URL to the file to download
    fileUrl <- c(.self$getPropValSlot('urls', 'base.url'), 'some', 'path',
           'to', 'the', 'file.zip')
    
    # Transform it intoa biodb URL object
    fileUrl <- BiodbUrl(url=fileUrl)

    # Download the file using the biodb scheduler
    .self$info("Downloading \"", fileUrl$toString(), "\"...")
    sched <- .self$getBiodb()$getRequestScheduler()
    sched$downloadFile(url=fileUrl, dest.file=.self$getDownloadPath())
}
# $$$ END_SECTION DOWNLOADABLE $$$
# $$$ SECTION DOWNLOADABLE $$$

,doExtractDownload=function() {

   .self$info("Extracting content of downloaded {{dbTitle}}...")
   cch <- .self$getBiodb()$getPersistentCache()

   # TODO Expand the downloaded files into a temporary folder
   extract.dir <- cch$getTmpFolderPath()
   filePath <- .self$getDownloadPath()
   # Here we unzip the file. TODO Replace with the appropriate processing
   .self$debug(paste("Unzipping ", filePath, "...", sep=''))
   utils::unzip(filePath, exdir=extract.dir)
   .self$debug(paste("Unzipped ", filePath, ".", sep=''))

    # Extract entries
    # TODO Do here the eventual needed processing to extract and/or transform
    # the individual entry files. There must be only one file for each entry.
    # The list of the files must be inside variable entryFiles
    entryFiles <- list()

    # Delete existing cache files
    .self$debug('Delete existing entry files in cache system.')
    cch$deleteFiles(.self$getCacheId(),
                    ext=.self$getPropertyValue('entry.content.type'))

    # Move the extracted entry files into the biodb cache folder
    ctype <- .self$getPropertyValue('entry.content.type')
    cch$moveFilesIntoCache(unname(entryFiles), cache.id=.self$getCacheId(),
                           name=names(entryFiles), ext=ctype)

    # Clean
    # TODO Do here any necessary cleaning among the remaining files written
    # inside the temporary folder.
}
# $$$ END_SECTION DOWNLOADABLE $$$
# $$$ SECTION WRITABLE $$$

,doWrite=function() {
    # Overrides super class' method.

    # TODO Update database on disk by writing new entries into it.
    # You have the choice between writing only the new entries or rewriting all
    # entries, depending on the type of the database (e.g.: CSV file or SQL
    # database for instance)

    # --- FIRST CHOICE --- WRITING DOWN NEW ENTRIES ONLY (SQL database case)
    # If you just need to write new entries, then loop on all entries in memory
    # and use the `isNew()` method on each entry to now its status. 
    newEntries <- Filter(function(e) e$isNew,
                         .self$getAllVolatileCacheEntries())
    # TODO Write the new entry to cache

    # TODO Choose between the scheme above or the scheme below

    # --- SECOND CHOICE --- REWRITING DOWN ALL ENTRIES (CSV file case)
    # Make sure all entries are loaded into cache.
    entry.ids <- .self$getEntryIds()
    entries <- .self$getBiodb()$getFactory()$getEntry(.self$getId(), entry.ids)

    # Get all entries: the ones loaded from the database file and the ones
    # created in memory (and not saved).
    allEntries <- .self$getAllCacheEntries()

    # Write all entries
    # TODO
}
# $$$ END_SECTION WRITABLE $$$
))
