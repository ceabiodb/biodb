# vi: fdm=marker ts=4 et cc=80 tw=80

# MassbankConn {{{1
################################################################################

#' @include BiodbRemotedbConn.R
#' @include BiodbMassdbConn.R
#' @include BiodbDownloadable.R
MassbankConn <- methods::setRefClass("MassbankConn",
    contains=c("BiodbRemotedbConn", "BiodbMassdbConn", 'BiodbDownloadable'),
    fields=list(
        .prefix2dns='list'
    ),

# Public methods {{{1
################################################################################

methods=list(

# Initialize {{{3
################################################################0

initialize=function(...) {

    callSuper(...)

    .self$.prefix2dns <- list()
},

# Do get mz values {{{3
################################################################################

.doGetMzValues=function(ms.mode, max.results, precursor, ms.level) {

    mz <- numeric(0)

    if ( ! is.null(ms.mode) && ! is.na(ms.mode))
        .self$message('debug', paste("ms.mode", ms.mode, sep='='))
    if ( ! is.null(max.results) && ! is.na(max.results))
        .self$message('debug', paste("max.results", max.results, sep='='))
    if ( ! is.null(precursor) && ! is.na(precursor))
        .self$message('debug', paste("precursor", precursor, sep='='))
    if ( ! is.null(ms.level) && ! is.na(ms.level))
        .self$message('debug', paste("ms.level", ms.level, sep='='))

    # Download
    .self$download()

    # Get list of spectra
    spectra.ids <- .self$getEntryIds()
    .self$message('debug', paste(length(spectra.ids), "spectra to process."))

    # Loop in all spectra
    i <- 0
    for (id in spectra.ids) {

        i <- i + 1
        .self$progressMsg("Processing entry", index=i,
                          total=length(spectra.ids), first=(i==1))

        # Get entry
        entry <- .self$getBiodb()$getFactory()$getEntry(.self$getId(), id)

        # Filter on mode
        if ( ! is.null(ms.mode) && ! is.na(ms.mode)
            && entry$getFieldValue('msmode') != ms.mode) {
            .self$debug("Reject entry ", id, " because MS mode is ",
                        entry$getFieldValue('msmode'))
            next
        }

        # Filter on ms.level
        if ( ! is.null(ms.level) && ! is.na(ms.level) && ms.level > 0
            && entry$getFieldValue('ms.level') != ms.level) {
            .self$debug("Reject entry ", id, " because MS level is ",
                        entry$getFieldValue('ms.level'))
            next
        }

        # Take mz values
        new.mz <- NULL
        peaks <- entry$getFieldValue('peaks')
        if ( ! is.null(peaks) && nrow(peaks) > 0
            && 'peak.mz' %in% colnames(peaks)) {
            new.mz <- peaks$peak.mz
            if (precursor && entry$hasField('msprecmz')) {
                prec.mz <- entry$getFieldValue('msprecmz', last=TRUE)
                new.mz <- if (prec.mz %in% new.mz) prec.mz else NULL
            }
        }

        # Add new M/Z values
        if ( ! is.null(new.mz)) {
            new.mz <- new.mz[ ! new.mz %in% mz]
            if (length(new.mz) > 0) {
                .self$debug("Add ", length(new.mz), " new M/Z values.")
                mz <- c(mz, new.mz)
            }
        }

        .self$message('debug', paste(length(mz), "M/Z values have been found."))

        # Stop if max reached
        if ( ! is.null(max.results) && ! is.na(max.results)
            && length(mz) >= max.results)
            break
    }

    # Cut
    if ( ! is.na(max.results) && length(mz) > max.results)
        mz <- mz[seq_len(max.results)]

    return(mz)
},

# Requires download {{{3
################################################################################

requiresDownload=function() {
    return(TRUE)
},

# Get entry content from database {{{3
################################################################################

getEntryContentFromDb=function(entry.id) {

    # NOTE Method unused, since database is downloaded.

    # Debug
    .self$debug("Get entry content(s) for ", length(entry.id)," entry.id(s)...")

    # Initialize return values
    content <- rep(NA_character_, length(entry.id))

    # Build request
    request <- paste0('<?xml version="1.0" encoding="UTF-8"?>',
'<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/"',
' xmlns:tns="http://api.massbank"><SOAP-ENV:Body><tns:getRecordInfo>',
paste(paste('<tns:entry.ids>', entry.id, '</tns:entry.ids>', sep=''),
      collapse=''), '</tns:getRecordInfo></SOAP-ENV:Body></SOAP-ENV:Envelope>')

    # Send request
    u <- c(.self$getPropValSlot('urls', 'base.url'), 'api', 'services',
           'MassBankAPI.MassBankAPIHttpSoap11Endpoint/')
    u <- BiodbUrl(url=u)$toString()
    xmlstr <- .self$getBiodb()$getRequestScheduler()$sendSoapRequest(u, request)

    # Parse XML and get text
    if ( ! is.na(xmlstr)) {
        xml <-  XML::xmlInternalTreeParse(xmlstr, asText=TRUE)
        ns <- c(ax21="http://api.massbank/xsd")
        returned.ids <- XML::xpathSApply(xml, "//ax21:id", XML::xmlValue,
                                         namespaces=ns)
        if (length(returned.ids) > 0) {
            c <- XML::xpathSApply(xml, "//ax21:info", XML::xmlValue,
                                  namespaces=ns)
            content[match(returned.ids, entry.id)] <- c
        }
    }

    return(content)
},

# Get nb entries {{{3
################################################################################

getNbEntries=function(count=FALSE) {
    return(length(.self$getEntryIds()))
},

# Get chromatographic columns {{{3
################################################################################

getChromCol=function(ids=NULL) {

    if (is.null(ids))
        ids <- .self$getEntryIds()

    # Get entries
    entries <- .self$getBiodb()$getFactory()$getEntry(.self$getId(), ids)

    # Get data frame
    fields <- c('chrom.col.id', 'chrom.col.name')
    cols <- .self$getBiodb()$entriesToDataframe(entries, fields=fields)
    if (is.null(cols))
        cols <- data.frame(chrom.col.id=character(), chrom.col.name=character())

    # Remove NA values
    cols <- cols[ ! is.na(cols$chrom.col.name), , drop=FALSE]

    # Remove duplicates
    cols <- cols[ ! duplicated(cols), ]

    # Rename columns
    names(cols) <- c('id', 'title')

    return(cols)
},

# Get dns from id {{{3
################################################################################

getDns=function(id) {

    # Download prefixes file, parse it and build list
    if (length(.self$.prefix2dns) == 0)
        .self$.loadPrefixes()

    dns <- vapply(id, function(x) {
        prefix <- sub('^([A-Z]+)[0-9]+$', '\\1', x, perl=TRUE)
        if (prefix %in% names(.self$.prefix2dns))
            .self$.prefix2dns[[prefix]]
        else
            NA_character_
        }, FUN.VALUE='', USE.NAMES=FALSE)

    return(dns)
},

# Get entry page url {{{3
################################################################################

getEntryPageUrl=function(id) {
    u <- c(.self$getPropValSlot('urls', 'base.url'), 'MassBank',
           'RecordDisplay.jsp')
    fct <- function(x) BiodbUrl(url=u,
                                params=list(id=x,
                                            dsn=.self$getDns(x)))$toString()
    return(vapply(id, fct, FUN.VALUE=''))
},

# Get entry image url {{{3
################################################################################

getEntryImageUrl=function(id) {
    return(rep(NA_character_, length(id)))
},

# Private methods {{{2
################################################################################

# Do download {{{3
################################################################################

.doDownload=function() {

    # Download tar.gz
    tar.gz.url <- BiodbUrl(url=.self$getPropValSlot('urls', 'db.tar.url'))
    .self$info("Downloading \"", tar.gz.url$toString(), "\"...")
    scheduler <- .self$getBiodb()$getRequestScheduler()
    path <- .self$getDownloadPath()
    scheduler$downloadFile(url=tar.gz.url, dest.file=path)
},

# Do extract download {{{3
################################################################################

.doExtractDownload=function() {

    # Extract
    extracted.dir <- tempfile(.self$getId())
    untar(tarfile=.self$getDownloadPath(), exdir=extracted.dir) 

    # Copy all exported files
    .self$info("Copy all extracted MassBank record files into cache.")
    record.files <- Sys.glob(file.path(extracted.dir, 'MassBank-data-master',
                                       '*', '*.txt'))
    .self$info("Found ", length(record.files),
               " record files in MassBank GitHub archive.")
    ids <- sub('^.*/([^/]*)\\.txt$', '\\1', record.files)
    dup.ids <- duplicated(ids)
    if (any(dup.ids))
        .self$caution("Found duplicated IDs in downloaded Massbank records: ",
                      paste(ids[dup.ids], collapse=', '), '.', sep='')
    cch <- .self$getBiodb()$getCache()
    ext <- .self$getPropertyValue('entry.content.type')
    cache.files <- cch$getFilePath(.self$getCacheId(), subfolder='shortterm',
                                   name=ids, ext=ext)
    cch$deleteFiles(.self$getCacheId(), subfolder='shortterm', ext=ext)
    file.copy(record.files, cache.files)

    # Delete extracted dir
    unlink(extracted.dir, recursive=TRUE)
},

# Do search M/Z range {{{3
################################################################################

.doSearchMzRange=function(mz.min, mz.max, min.rel.int, ms.mode, max.results,
                          precursor, ms.level) {
    mz <- (mz.min + mz.max) / 2
    mz.tol <- mz.max - mz
    return(.self$searchMzTol(mz=mz, mz.tol=mz.tol, mz.tol.unit='plain',
                             min.rel.int=min.rel.int, ms.mode=ms.mode,
                             max.results=max.results, precursor=precursor,
                             ms.level=ms.level))
},

# Do search M/Z with tolerance {{{3
################################################################################

.doSearchMzTol=function(mz, mz.tol, mz.tol.unit, min.rel.int, ms.mode,
                        max.results, precursor, ms.level) {

    returned.ids <- character()
    sched <- .self$getBiodb()$getRequestScheduler()

    # Multiple M/Z values and PPM tolerance
    if (length(mz) > 1 && mz.tol.unit == 'ppm') {
        for (mz.single in mz) {
            ids <- .self$.doSearchMzTol(mz=mz.single, mz.tol=mz.tol,
                                        mz.tol.unit=mz.tol.unit,
                                        min.rel.int=min.rel.int,
                                        ms.mode=ms.mode,
                                        max.results=max.results,
                                        precursor=precursor, ms.level=ms.level)
            returned.ids <- c(returned.ids, ids)
        }
        returned.ids <- returned.ids[ ! duplicated(returned.ids)]
    }

    # Single M/Z value or PLAIN tolerance
    else {

        # Set tolerance
        if (mz.tol.unit == 'ppm')
            mz.tol <- mz.tol * mz * 1e-6

        # Build request
        max <- max.results
        if ( ! is.na(max) && (precursor || ms.level > 0))
            max <- max(10000, 10 * max)
        xml.request <- paste0('<?xml version="1.0" encoding="UTF-8"?>',
'<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/"',
' xmlns:tns="http://api.massbank"><SOAP-ENV:Body><tns:searchPeak><tns:mzs>',
paste(mz, collapse=','), '</tns:mzs><tns:relativeIntensity>',
if (is.na(min.rel.int)) 0 else (min.rel.int * 999 %/% 100),
'</tns:relativeIntensity><tns:tolerance>', mz.tol,
'</tns:tolerance><tns:instrumentTypes>all</tns:instrumentTypes><tns:ionMode>',
if (is.na(ms.mode)) 'Both'
else ( if (ms.mode == 'neg') 'Negative' else 'Positive'),
'</tns:ionMode><tns:maxNumResults>', if (is.na(max)) 0 else max,
'</tns:maxNumResults></tns:searchPeak></SOAP-ENV:Body></SOAP-ENV:Envelope>')

        # Send request
        .self$debug('Searching for M/Z values, with request: "', xml.request,
                    '".')
        u <- c(.self$getPropValSlot('urls', 'base.url'), 'api', 'services',
               'MassBankAPI.MassBankAPIHttpSoap11Endpoint/')
        u <- BiodbUrl(url=u)$toString()
        xmlstr <- sched$sendSoapRequest(u, xml.request)

        # Parse XML and get text
        if ( ! is.na(xmlstr)) {
            .self$message('debug', 'Parsing XML response to get IDs.')
            xml <-  XML::xmlInternalTreeParse(xmlstr, asText=TRUE)
            ns <- c(ax21="http://api.massbank/xsd")
            returned.ids <- XML::xpathSApply(xml, "//ax21:id", XML::xmlValue,
                                             namespaces=ns)
            .self$debug('Found spectra ', paste(returned.ids, collapse=', '),
                        '.')

            if (ms.level > 0 || precursor) {

                # Get entries
                .self$debug('Get entries')
                entries <- .self$getEntry(returned.ids, drop=FALSE)

                # Filter on precursor
                if (precursor) {
                    .self$debug('Filtering on precurssor ', precursor, '.')
                    fct <- function(x) {
                        if (is.null(x))
                            NA_real_
                        else
                            x$getFieldValue('msprecmz', last=TRUE)
                    }
                    precursor.mz <- vapply(entries, fct, FUN.VALUE=1.0)
                    precursor.matched <- ((! is.na(precursor.mz))
                                          & (precursor.mz >= mz - mz.tol)
                                          & (precursor.mz <= mz + mz.tol))
                    entries <- entries[precursor.matched]
                    .self$debug(length(entries), ' entrie(s) left.')
                }

                # Filter on ms.level
                if (ms.level > 0) {
                    .self$debug('Filtering on MS level ', ms.level, '.')
                    fct <- function(x) {
                        if (is.null(x))
                            FALSE
                        else
                            x$getFieldValue('MS.LEVEL') == ms.level
                    }
                    ms.level.matched <- vapply(entries, fct, FUN.VALUE=TRUE)
                    entries <- entries[ms.level.matched]
                    .self$debug(length(entries), ' entrie(s) left.')
                }

                .self$message('debug', 'Getting list of IDs.')
                fct <- function(x) x$getFieldValue('ACCESSION')
                returned.ids <- vapply(entries, fct, FUN.VALUE='')
                .self$debug('Remaining spectra are ',
                            paste(returned.ids, collapse=', '), '.')
            }
        }
    }

    # Cut
    if ( ! is.na(max.results) && length(returned.ids) > max.results) {
        .self$message('debug', 'Cut list of IDs to return.')
        returned.ids <- returned.ids[seq_len(max.results)]
    }

    return(returned.ids)
},

# Load prefixes {{{3
################################################################################

.loadPrefixes=function() {

    cch <- .self$getBiodb()$getCache()
    sched <- .self$getBiodb()$getRequestScheduler()

    # Get prefixes file content
    prefixes.filepath <- cch$getFilePath(.self$getCacheId(),
                                         subfolder='longterm', name='prefixes',
                                         ext='md')
    if ( ! file.exists(prefixes.filepath)) {
        u <- BiodbUrl(url=.self$getPropValSlot('urls', 'prefixes.file.url'))
        sched$downloadFile(url=u, dest.file=prefixes.filepath)
    }

    # Split in lines
    lines <- readLines(prefixes.filepath)

    # Skip header lines
    lines <- lines[3:length(lines)]

    # Parse databases and prefixes
    re <- '^\\| *([^ ]+) +\\|[^|]+\\|[^|]+\\| *([A-Z ,]+) *\\|.*$'
    results <- stringr::str_match_all(lines, re)
    dbs <- vapply(results, function(x) x[1,2], FUN.VALUE='')
    fct <- function(x) stringr::str_match_all(x[1,3], '([A-Z]+)')[[1]][,1]
    prefixes <- lapply(results, fct)

    # Loop on all databases
    for (i in seq_along(dbs)) {

        # Loop on prefixes
        for (p in prefixes[[i]])
            .self$.prefix2dns[[p]] <- dbs[[i]]
    }
},

# Get entry ids {{{3
################################################################################

.doGetEntryIds=function(max.results=NA_integer_, ms.level=0) {

    cch <- .self$getBiodb()$getCache()
    
    # Download
    .self$download()

    # Get IDs from cache
    ext <- .self$getPropertyValue('entry.content.type')
    ids <- cch$listFiles(.self$getCacheId(), subfolder='shortterm', ext=ext,
                         extract.name=TRUE)

    # Filter on MS level
    if ( ! is.na(ms.level) && ms.level > 0) {
        new.ids <- character(0)
        for (id in ids) {
            entry <- .self$getBiodb()$getFactory()$getEntry(.self$getId(), id)
            if (entry$getFieldValue('ms.level') == ms.level)
                new.ids <- c(new.ids, id)
            if ( ! is.na(max.results) && length(new.ids) >= max.results)
                break
        }
        ids <- new.ids
    }

    return(ids)
}

))
