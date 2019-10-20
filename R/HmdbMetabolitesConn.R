# vi: fdm=marker ts=4 et cc=80 tw=80

# HmdbMetabolitesConn {{{1
################################################################################

# Declaration {{{2
################################################################################

#' The connector class for the HMDB Metabolites database.
#'
#' This is a concrete connector class. It must never be instantiated directly,
#' but instead be instantiated through the factory \code{\link{BiodbFactory}}.
#' Only specific methods are described here. See super classes for the
#' description of inherited methods.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('hmdb.metabolites')
#'
#' # Get an entry
#' \dontrun{
#' e <- conn$getEntry('HMDB0000001')
#' }
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include BiodbRemotedbConn.R
#' @include BiodbCompounddbConn.R
#' @include BiodbDownloadable.R
HmdbMetabolitesConn <- methods::setRefClass("HmdbMetabolitesConn",
    contains=c("BiodbRemotedbConn", "BiodbCompounddbConn", 'BiodbDownloadable'),
    fields=list(
        .ns="character"
        ),

# Public methods {{{2
################################################################################

methods=list(

# Get nb entries {{{3
################################################################################

getNbEntries=function(count=FALSE) {
    # Overrides super class' method.

    n <- NA_integer_

    ids <- .self$getEntryIds()
    if ( ! is.null(ids))
        n <- length(ids)

    return(n)
},

# Get entry page url {{{3
################################################################################

getEntryPageUrl=function(id) {
    # Overrides super class' method.
    
    fct <- function(x) {
        u <- c(.self$getPropValSlot('urls', 'base.url'), 'metabolites', x)
        BiodbUrl(url=u)$toString()
    }
    
    return(vapply(id, fct, FUN.VALUE=''))
},

# Get entry image url {{{3
################################################################################

getEntryImageUrl=function(id) {
    # Overrides super class' method.

    fct <- function(x) {
        u <- c(.self$getPropValSlot('urls', 'base.url'), 'structures', x,
               'image.png')
        BiodbUrl(url=u)$toString()
    }

    return(vapply(id, fct, FUN.VALUE=''))
},


# Private methods {{{2
################################################################################

# Do get entry content request {{{3
################################################################################

.doGetEntryContentRequest=function(id, concatenate=TRUE) {

    u <- c(.self$getPropValSlot('urls', 'base.url'), 'metabolites',
           paste(id, 'xml', sep='.'))
    url <- BiodbUrl(url=u)$toString()

    return(url)
},

# Do download {{{3
################################################################################

.doDownload=function() {

    # Download
    .self$message('info', "Downloading HMDB metabolite database...")
    u <- c(.self$getPropValSlot('urls', 'base.url'), 'system', 'downloads',
           'current', 'hmdb_metabolites.zip')
    zip.url <- BiodbUrl(url=u)
    .self$info("Downloading \"", zip.url$toString(), "\"...")
    sched <- .self$getBiodb()$getRequestScheduler()
    sched$downloadFile(url=zip.url, dest.file=.self$getDownloadPath())
},

# Do extract download {{{3
################################################################################

.doExtractDownload=function() {

    .self$info("Extracting content of downloaded HMDB metabolite database...")
    cch <- .self$getBiodb()$getPersistentCache()

    # Expand zip
    extract.dir <- tempfile(.self$getId())
    zip.path <- .self$getDownloadPath()
    .self$message('debug', paste("Unzipping ", zip.path, "...", sep=''))
    utils::unzip(zip.path, exdir=extract.dir)
    .self$message('debug', paste("Unzipped ", zip.path, ".", sep=''))
    
    # Search for extracted XML file
    files <- list.files(path=extract.dir)
    xml.file <- NULL
    if (length(files) == 0)
        .self$error("No XML file found in zip file \"", .self$getDownloadPath(),
                    "\".")
    else if (length(files) == 1)
        xml.file <- file.path(extract.dir, files)
    else {
        for (f in c('hmdb_metabolites.xml', 'hmdb_metabolites_tmp.xml'))
            if (f %in% files)
                xml.file <- file.path(extract.dir, f)
        if (is.null(xml.file))
            .self$error("More than one file found in zip file \"",
                        .self$getDownloadPath(), "\":",
                        paste(files, collapse=", "), ".")
    }
    if (is.null(xml.file))
        .self$error("No XML file found in ZIP file.")
    .self$debug("Found XML file ", xml.file, " in ZIP file.")

    # Delete existing cache files
    .self$message('debug', 'Delete existing entry files in cache system.')
    cch$deleteFiles(.self$getCacheId(),
                    ext=.self$getPropertyValue('entry.content.type'))

    # Open file in binary mode
    file.conn <- file(xml.file, open='rb')

    # Extract entries from XML file
    .self$message('debug', "Extract entries from XML file.")
    chunk.size <- 2**16
    total.bytes <- file.info(xml.file)$size
    bytes.read <- 0
    xml.chunks <- character()
    .self$debug("Read XML file by chunk of", chunk.size, "characters.")
    done.reading <- FALSE
    while ( ! done.reading) {

        first <- (bytes.read == 0)

        # Read chunk from file
        chunk <- readChar(file.conn, chunk.size)
        done.reading <- (nchar(chunk) < chunk.size)

        # Send progress message
        bytes.read <- bytes.read + nchar(chunk, type='bytes')
        .self$progressMsg(msg='Reading all HMDB metabolites from XML file.',
                          index=bytes.read, total=total.bytes, first=first)

        # Is there a complete entry XML (<metabolite>...</metabolite>) in the
        # loaded chunks?
        if (length(grep('</metabolite>', chunk)) > 0
            || (length(xml.chunks) > 0
                && length(grep('</metabolite>',
                               paste0(xml.chunks[[length(xml.chunks)]],
                                      chunk))) > 0)) {

            # Paste all chunks together
            xml <- paste(c(xml.chunks, chunk), collapse='')

            # Search entry definitions
            re <- stringr::regex('^(.*?)(<metabolite>.*</metabolite>)(.*)$',
                                 dotall=TRUE)
            match <- stringr::str_match(xml, re)
            if (is.na(match[1, 1]))
                .self$error('Cannot find matching <metabolite> tag in HMDB',
                            ' XML entries file.')
            metabolites <- match[1, 3]
            xml.chunks <- match[1, 4]

            # Get all metabolites definition
            re <- stringr::regex('<metabolite>.*?</metabolite>', dotall=TRUE)
            metabolites <- stringr::str_extract_all(metabolites, re)[[1]]

            # Get IDs
            re <- '<accession>(HMDB[0-9]+)</accession>'
            ids <- stringr::str_match(metabolites, re)[, 2]

            # Write all XML entries into files
            ctype <- .self$getPropertyValue('entry.content.type')
            cch$saveContentToFile(metabolites, cache.id=.self$getCacheId(),
                                  name=ids, ext=ctype)
        }
        else
            xml.chunks <- c(xml.chunks, chunk)
    }

    # Close file
    close(file.conn)

    # Remove extract directory
    .self$message('debug', 'Delete extract directory.')
    unlink(extract.dir, recursive=TRUE)
},

# Get entry ids {{{3
################################################################################

.doGetEntryIds=function(max.results=NA_integer_) {

    ids <- NULL
    cch <- .self$getBiodb()$getPersistentCache()

    # Download
    .self$download()

    if (.self$isDownloaded()) {

        # Get IDs from cache
        ctype <- .self$getPropertyValue('entry.content.type')
        ids <- cch$listFiles(.self$getCacheId(),
                             ext=ctype, extract.name=TRUE)

        # Filter out wrong IDs
        ids <- ids[grepl("^HMDB[0-9]+$", ids, perl=TRUE)]
    }

    return(ids)
}

))
