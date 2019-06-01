# vi: fdm=marker ts=4 et cc=80

# Class declaration {{{1
################################################################################

#' @include BiodbRemotedbConn.R
#' @include BiodbCompounddbConn.R
#' @include BiodbDownloadable.R
HmdbMetabolitesConn <- methods::setRefClass("HmdbMetabolitesConn", contains = c("BiodbRemotedbConn", "BiodbCompounddbConn", 'BiodbDownloadable'), fields = list(.ns = "character"))

# Initialize {{{1
################################################################################

HmdbMetabolitesConn$methods( initialize = function(...) {

    callSuper(...)

    .self$.setDownloadExt('zip')
})

# Get nb entries {{{1
################################################################################

HmdbMetabolitesConn$methods( getNbEntries = function(count = FALSE) {

    n <- NA_integer_

    ids <- .self$getEntryIds()
    if ( ! is.null(ids))
        n <- length(ids)

    return(n)
})

# Get entry page url {{{1
################################################################################

HmdbMetabolitesConn$methods( getEntryPageUrl = function(id) {
    return(vapply(id, function(x) BiodbUrl(url = c(.self$getPropValSlot('urls', 'base.url'), 'metabolites', x))$toString(), FUN.VALUE = ''))
})

# Get entry image url {{{1
################################################################################

HmdbMetabolitesConn$methods( getEntryImageUrl = function(id) {
    return(vapply(id, function(x) BiodbUrl(url = c(.self$getPropValSlot('urls', 'base.url'), 'structures', x, 'image.png'))$toString(), FUN.VALUE = ''))
})

# Search compound {{{1
################################################################################

HmdbMetabolitesConn$methods( searchCompound = function(name = NULL, mass = NULL, mass.field = NULL, mass.tol = 0.01, mass.tol.unit = 'plain', max.results = NA_integer_) {
        
    .self$.checkMassField(mass = mass, mass.field = mass.field)

    ids <- NULL

    .self$message('caution', 'HMDB is not searchable. HMDB only provides an HTML interface for searching, giving results split across several pages. It is unpractical to use from a program. Since HMDB is downloaded entirely, a solution using an internal database will be implemented in the future.')

    return(ids)
})

# Private methods {{{1
################################################################################

# Do get entry content request {{{2
################################################################################

HmdbMetabolitesConn$methods( .doGetEntryContentRequest = function(id, concatenate = TRUE) {

    url <- BiodbUrl(url = c(.self$getPropValSlot('urls', 'base.url'), 'metabolites', paste(id, 'xml', sep = '.')))$toString()

    return(url)
})

# Do download {{{2
################################################################################

HmdbMetabolitesConn$methods( .doDownload = function() {

    # Download
    .self$message('info', "Downloading HMDB metabolite database...")
    zip.url <- BiodbUrl(url = c(.self$getPropValSlot('urls', 'base.url'), 'system', 'downloads', 'current', 'hmdb_metabolites.zip'))
    .self$message('info', paste("Downloading \"", zip.url$toString(), "\"...", sep = ''))
    .self$getBiodb()$getRequestScheduler()$downloadFile(url = zip.url, dest.file = .self$getDownloadPath())
})

# Do extract download {{{2
################################################################################

HmdbMetabolitesConn$methods( .doExtractDownload = function() {

    .self$message('info', "Extracting content of downloaded HMDB metabolite database...")

    # Expand zip
    extract.dir <- tempfile(.self$getId())
    zip.path <- .self$getDownloadPath()
    .self$message('debug', paste("Unzipping ", zip.path, "...", sep = ''))
    utils::unzip(zip.path, exdir = extract.dir)
    .self$message('debug', paste("Unzipped ", zip.path, ".", sep  = ''))
    
    # Search for extracted XML file
    files <- list.files(path = extract.dir)
    xml.file <- NULL
    if (length(files) == 0)
        .self$message('error', paste("No XML file found in zip file \"", .self$getDownloadPath(), "\".", sep = ''))
    else if (length(files) == 1)
        xml.file <- file.path(extract.dir, files)
    else {
        for (f in c('hmdb_metabolites.xml', 'hmdb_metabolites_tmp.xml'))
            if (f %in% files)
                xml.file <- file.path(extract.dir, f)
        if (is.null(xml.file))
            .self$message('error', paste("More than one file found in zip file \"", .self$getDownloadPath(), "\":", paste(files, collapse = ", "), ".", sep = ''))
    }
    if (is.null(xml.file))
        .self$message('error', "No XML file found in ZIP file.")
    .self$message('debug', paste("Found XML file ", xml.file, " in ZIP file.", sep  = ''))

    # Delete existing cache files
    .self$message('debug', 'Delete existing entry files in cache system.')
    .self$getBiodb()$getCache()$deleteFiles(.self$getCacheId(), subfolder = 'shortterm', ext = .self$getPropertyValue('entry.content.type'))

    # Open file in binary mode
    file.conn <- file(xml.file, open = 'rb')

    # Extract entries from XML file
    .self$message('debug', "Extract entries from XML file.")
    chunk.size <- 2**16
    total.bytes <- file.info(xml.file)$size
    bytes.read = 0
    xml.chunks = character()
    .self$message('debug', paste("Read XML file by chunk of", chunk.size, "characters."))
    done.reading = FALSE
    while ( ! done.reading) {

        first = (bytes.read == 0)

        # Read chunk from file
        chunk <- readChar(file.conn, chunk.size)
        done.reading <- (nchar(chunk) < chunk.size)

        # Send progress message
        bytes.read <- bytes.read + nchar(chunk, type = 'bytes')
        lapply(.self$getBiodb()$getObservers(), function(x) x$progress(type = 'info', msg = 'Reading all HMDB metabolites from XML file.', index = bytes.read, total = total.bytes, first = first))

        # Is there a complete entry XML (<metabolite>...</metabolite>) in the loaded chunks?
        if (length(grep('</metabolite>', chunk)) > 0 || (length(xml.chunks) > 0 && length(grep('</metabolite>', paste0(xml.chunks[[length(xml.chunks)]], chunk))) > 0)) {

            # Paste all chunks together
            xml <- paste(c(xml.chunks, chunk), collapse = '')

            # Search entry definitions
            match <- stringr::str_match(xml, stringr::regex('^(.*?)(<metabolite>.*</metabolite>)(.*)$', dotall = TRUE))
            if (is.na(match[1, 1]))
                .self$message('error', 'Cannot find matching <metabolite> tag in HMDB XML entries file.')
            metabolites <- match[1, 3]
            xml.chunks <- match[1, 4]

            # Get all metabolites definition
            metabolites <- stringr::str_extract_all(metabolites, stringr::regex('<metabolite>.*?</metabolite>', dotall = TRUE))[[1]]

            # Get IDs
            ids <- stringr::str_match(metabolites, '<accession>(HMDB[0-9]+)</accession>')[, 2]

            # Write all XML entries into files
            .self$getBiodb()$getCache()$saveContentToFile(metabolites, cache.id = .self$getCacheId(), subfolder = 'shortterm', name = ids, ext = .self$getPropertyValue('entry.content.type'))
        }
        else
            xml.chunks <- c(xml.chunks, chunk)
    }

    # Close file
    close(file.conn)

    # Remove extract directory
    .self$message('debug', 'Delete extract directory.')
    unlink(extract.dir, recursive = TRUE)
})

# Get entry ids {{{2
################################################################################

HmdbMetabolitesConn$methods( .doGetEntryIds = function(max.results = NA_integer_) {

    ids <- NULL

    # Download
    .self$download()

    if (.self$isDownloaded()) {

        # Get IDs from cache
        ids <- .self$getBiodb()$getCache()$listFiles(.self$getCacheId(), subfolder = 'shortterm', ext = .self$getPropertyValue('entry.content.type'), extract.name = TRUE)

        # Filter out wrong IDs
        ids <- ids[grepl("^HMDB[0-9]+$", ids, perl = TRUE)]
    }

    return(ids)
})

