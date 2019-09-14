# vi: fdm=marker ts=4 et cc=80 tw=80

# PeakforestMassConn {{{1
################################################################################

# Declaration {{{2
################################################################################

#' PeakForest Mass connector class.
#'
#' This is the connector class for PeakForest Mass database.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('peakforest.mass')
#'
#' # Get an entry
#' e <- conn$getEntry('1000')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include PeakforestConn.R
#' @include BiodbMassdbConn.R
#' @export PeakforestMassConn
#' @exportClass PeakforestMassConn
PeakforestMassConn <- methods::setRefClass("PeakforestMassConn",
    contains=c("PeakforestConn", "BiodbMassdbConn"),

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(...) {

    callSuper(db.name='spectra/lcms', ...)
},

# Get entry page url {{{3
################################################################################

getEntryPageUrl=function(id) {
    # Overrides super class' method.

    u <- .self$getPropValSlot('urls', 'base.url')
    f <- function(x) BiodbUrl(url=u, params=list(PFs=x))$toString()
    return(vapply(id, f, FUN.VALUE=''))
},


# Peaks get range {{{2
################################################################################

wsPeaksGetRange=function(type=c('lcms', 'lcmsms'), mz.min, mz.max,
                         mode=NA_character_,
                         retfmt=c('plain', 'request', 'parsed', 'ids')) {
    ":\n\nCalls the spectra/<type>/peaks/get-range web service to search for
    spectra containing at least one peak whose M/Z value is inside a range.
    \ntype: The type of mass database: either 'lcms' or 'lcmsms'.
    \nmz.min: The minimum M/Z value to search for.
    \nmz.max: The maximum M/Z value to search for.
    \nmode: The MS mode: either 'NEG' or 'POS'. If unset, the search will be
    made on both modes.
    \nretfmt: Use to set the format of the returned value. 'plain' will return
    the raw results from the server, as a character value. 'parsed' will return
    the parsed results, as a JSON object. 'request' will return a BiodbRequest
    object representing the request as it would have been sent. 'ids' will
    return a character vector containing the IDs of the matching entries.
    \nReturned value: Depending on `retfmt` parameter.
    "

    type <- match.arg(type)
    retfmt <- match.arg(retfmt)

    results <- character()

    if ( ! is.null(mz.min) && ! is.null(mz.max) && ! is.na(mz.min)
        && ! is.na(mz.max)) {

        # Build request
        params <- c(token=.self$getPropertyValue('token'))
        if ( ! is.na(mode))
            params <- c(params, mode=mode)
        u <- c(.self$getPropValSlot('urls', 'ws.url'), "spectra", type, "peaks",
               "get-range", mz.min, mz.max)
        url <- BiodbUrl(url=u, params=params)
        request <- BiodbRequest(method='get', url=url)
        if (retfmt == 'request')
            return(request)

        # Send request
        results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

        # Parse
        results <- .self$.parseResults(results, retfmt=retfmt)
    }

    return(results)
},

# Webservice lcmsms/from-precursor
################################################################################

wsLcmsmsFromPrecursor=function(prec.mz, precursorMassDelta, mode=NA_character_,
                               retfmt=c('plain', 'request', 'parsed', 'ids')) {
    ":\n\nCalls the spectra/lcmsms/from-precursor web service to search for
    spectra containing a specific precursor peak.
    \nprec.mz: M/Z value of the precursor.
    \nprecursorMassDelta: The tolerance of the precursor M/Z value.
    \nmode: The MS mode: either 'NEG' or 'POS'. If unset, the search will be
    made on both modes.
    \nretfmt: Use to set the format of the returned value. 'plain' will return
    the raw results from the server, as a character value. 'parsed' will return
    the parsed results, as a JSON object. 'request' will return a BiodbRequest
    object representing the request as it would have been sent. 'ids' will
    return a character vector containing the IDs of the matching entries.
    \nReturned value: Depending on `retfmt` parameter.
    "

    retfmt <- match.arg(retfmt)

    results <- character()

    if ( ! is.null(prec.mz) && ! is.na(prec.mz)) {

        # Build request
        params <- c(token=.self$getPropertyValue('token'),
                    precursorMassDelta=precursorMassDelta)
        if ( ! is.na(mode))
            params <- c(params, mode=mode)
        u <- c(.self$getPropValSlot('urls', 'ws.url'), 'spectra', 'lcmsms',
               'from-precursor', prec.mz)
        url <- BiodbUrl(url=u, params=params)
        request <- BiodbRequest(method='get', url=url)
        if (retfmt == 'request')
            return(request)

        # Send request
        results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

        # Parse
        results <- .self$.parseResults(results, retfmt=retfmt)
    }

    return(results)
},

# Web service list-code-columns {{{3
################################################################################

wsListCodeColumns=function(retfmt=c('plain', 'request', 'parsed',
                                    'data.frame')) {
    ":\n\nCalls the metadata/lc/list-code-columns web service to get a list of
    available chromatographic columns.
    \nretfmt: Use to set the format of the returned value. 'plain' will return
    the raw results from the server, as a character value. 'parsed' will return
    the parsed results, as a JSON object. 'request' will return a BiodbRequest
    object representing the request as it would have been sent. 'data.frame'
    will return a data frame.
    \nReturned value: Depending on `retfmt` parameter.
    "

    retfmt <- match.arg(retfmt)

    # Build request
    u <- c(.self$getPropValSlot('urls', 'ws.url'), 'metadata', 'lc',
           'list-code-columns')
    url <- BiodbUrl(url=u, params=list(token=.self$getPropertyValue('token')))
    request <- BiodbRequest(method='get', url=url)
    if (retfmt == 'request')
        return(request)

    # Send request
    results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

    # Parse
    if (retfmt != 'plain') {

        # Check JSON
        if ( ! jsonlite::validate(results))
            .self$error("Invalid JSON returned by server.")
            
        # Parse results
        results <- jsonlite::fromJSON(results, simplifyDataFrame=FALSE)

        # Build data frame
        if (retfmt == 'data.frame') {
            cols <- data.frame(id=character(), title=character())
            for(id in names(results)) {
                col <- results[[id]]

                # Make title
                title <- ""
                col.fields <- list(constructor='', name='', length='L',
                                   diameter='diam', flow_rate='FR',
                                   particule_size='PS')
                for (field in names(col.fields))
                    if (field %in% names(col)) {
                        if (nchar(title) == 0)
                            title <- paste(col.fields[[field]], col[[field]])
                        else
                            title <- paste(title, col.fields[[field]],
                                           col[[field]])
                    }

                # Add col to data frame
                cols <- rbind(cols, data.frame(id=id, title=title,
                                               stringsAsFactors=FALSE))
            }
            .self$debug('Found ', nrow(cols), ' chromatographic columns.')
            results <- cols
        }
    }

    return(results)
},

# Get chromatographic columns {{{3
################################################################################

getChromCol=function(ids=NULL) {
    # Overrides super class' method.

    cols <- .self$wsListCodeColumns(retfmt='data.frame')
    bdb <- .self$getBiodb()

    # Restrict to set of spectra
    if ( ! is.null(ids)) {
        entries <- .self$getBiodb()$getFactory()$getEntry(.self$getId(), ids)
        selected.cols <- bdb$entriesToDataframe(entries, fields='chrom.col.id',
                                                drop=TRUE)

        # Filter cols data frame
        cols <- cols[cols$id %in% selected.cols, ]

        .self$debug('Restricted set of chromatographic columns to ', nrow(cols),
                    ' after filtering on spectra IDs.')
    }

    return(cols)
},

# Private methods {{{2
################################################################################

# Get mz values {{{3
################################################################################

.doGetMzValues=function(ms.mode, max.results, precursor, ms.level) {

    mz <- NULL
    if (ms.level > 0 || precursor) {

        # Get all IDs
        ids <- .self$getEntryIds()

        # Loop on all IDs
        for (id in ids) {

            entry <- .self$getBiodb()$getFactory()$getEntry(.self$getId(), id)
            .self$.assertNotNull(entry)

            if (ms.level > 0
                && ( ! entry$hasField('ms.level')
                    || entry$getFieldValue('ms.level') != ms.level))
                next

            if (precursor) {
                if (entry$hasField('msprecmz'))
                    mz <- c(mz, entry$getFieldValue('msprecmz'))
            }
            else {
                if (entry$hasField('peaks'))
                    mz <- c(mz, entry$getFieldValue('peaks')[['peak.mz']])
            }

            if ( ! is.na(max.results) && length(mz) > max.results)
                break
        }
    }

    else {
        # Set URL
        u <- c(.self$getPropValSlot('urls', 'ws.url'), 'spectra', 'lcms',
               'peaks', 'list-mz')
        p <- c(token=.self$getPropertyValue('token'))
        if ( ! is.na(ms.mode)) {
            m <- if (ms.mode == 'pos') 'positive' else 'negative'
            p <- c(p, mode=m)
        }
        url <- BiodbUrl(url=u, params=p)
        url <- url$toString()

        # Get MZ values
        jsonStr <- .self$getBiodb()$getRequestScheduler()$getUrl(url)
        .self$.checkIfError(jsonStr)

        # Check JSON
        if ( ! jsonlite::validate(jsonStr))
            .self$error("Invalid JSON returned by server.")

        # Parse JSON
        mz <- jsonlite::fromJSON(jsonStr, simplifyDataFrame=FALSE)
    }

    # Apply cut-off
    if ( ! is.na(max.results) && length(mz) > max.results)
        mz <- mz[seq_len(max.results)]

    return(mz)
},

# Parse IDs from JSON {{{3
################################################################################

.parseResults=function(results, retfmt) {

    if (retfmt != 'plain') {

        # Check JSON
        if ( ! jsonlite::validate(results))
            .self$error("Invalid JSON returned by server.")

        # Parse
        results <- jsonlite::fromJSON(results, simplifyDataFrame=FALSE)

        if(retfmt == 'ids') {

            if (length(results) == 0)
                results <- character()

            # Extract IDs
            else {
                f <- function(x) {
                    if ('id' %in% names(x))
                        x$id
                    else if ('source' %in% names(x) && ! is.null(x$source)
                             && 'id' %in% names(x$source))
                        x$source$id
                    else
                        NA_integer_
                }
                ids <- vapply(results, f, FUN.VALUE=1)
                
                # Remove NA values (some returned matches have no source
                # information).
                ids <- ids[ ! is.na(ids)]
                
                ids <- unlist(ids)
                results <- as.character(ids)
            }
        }
    }

    return(results)
},



# Do search M/Z range {{{3
################################################################################

.doSearchMzRange=function(mz.min, mz.max, min.rel.int, ms.mode, max.results,
                          precursor, ms.level) {

    ids <- character()

    # Multiple M/Z ranges
    if (length(mz.min) > 1) {
        for (i in seq_along(mz.min)) {
            x <- .self$.doSearchMzRange(mz.min=mz.min[[i]], mz.max=mz.max[[i]],
                                        min.rel.int=min.rel.int,
                                        ms.mode=ms.mode,
                                        max.results=max.results,
                                        precursor=precursor, ms.level=ms.level)
            ids <- c(ids, x)
        }
        ids <- ids[ ! duplicated(ids)]
    }

    # Single M/Z range
    else {
        mode <- NA_character_
        if ( ! is.na(ms.mode))
            mode <- (if (ms.mode == 'neg') 'NEG' else 'POS')
        if (ms.level == 0 || ms.level == 1)
            ids <- .self$wsPeaksGetRange('lcms', mz.min, mz.max, mode=mode,
                                         retfmt='ids')
        if (ms.level == 0 || ms.level == 2) {
            if (precursor) {
                x <- .self$wsLcmsmsFromPrecursor((mz.min + mz.max) / 2,
                                                 (mz.max - mz.min) / 2,
                                                 mode=mode, retfmt='ids')
                ids <- c(ids, x)
            }
            else {
                x <- .self$wsPeaksGetRange('lcmsms', mz.min, mz.max, mode=mode,
                                           retfmt='ids')
                ids <- c(ids, x)
            }
        }
    }

    # Filtering
    if (length(ids) > 0 && ( length(mz.min) > 1 || ! is.na(min.rel.int)
                            || precursor)) {

        entries <- .self$getBiodb()$getFactory()$getEntry(.self$getId(), ids,
                                                          drop=FALSE)

        # Filter on precursor
        if (precursor && (ms.level %in% c(0, 1) || length(mz.min > 1))) {
            f <- function(e) {
                if ( ! is.null(e) && e$hasField('msprecmz')
                    && any(e$getFieldValue('msprecmz') <= mz.max
                           & e$getFieldValue('msprecmz') >= mz.min))
                    e
                else
                    NULL
            }
            entries <- lapply(entries, f)
        }

        # Filter on M/Z ranges when there are more than one range, and also on
        # intensity
        if (length(mz.min) > 1 || ! is.na(min.rel.int)) {
            filtered.entries <- NULL
            for (e in entries) {

                good.entry <- FALSE
                if (! is.null(e) && e$hasField('peaks')) {
                    peaks <- e$getFieldValue('peaks')
                    good.entry <- TRUE
                    for (i in seq_along(mz.min)) {
                        in.range <- (peaks[['peak.mz']] >= mz.min[[i]]
                                     & peaks[['peak.mz']] <= mz.max[[i]])
                        if ( ! any(in.range)
                            || ( ! is.na(min.rel.int)
                                && ! any(peaks[in.range,
                                         'peak.relative.intensity']
                                >= min.rel.int))) {
                            good.entry <- FALSE
                            break
                        }
                    }
                }

                # Append entry to list
                filtered.entries <- c(filtered.entries,
                                      if (good.entry) e else list(NULL))
            }
            entries <- filtered.entries
        }

        # Select entries
        ids <- ids[ ! vapply(entries, is.null, FUN.VALUE=TRUE)]
    }

    # Cut
    if ( ! is.na(max.results) && length(ids) > max.results)
        ids <- ids[seq_len(max.results)]

    return(ids)
},

# Do get entry content request {{{3
################################################################################

.doGetEntryContentRequest=function(id, concatenate=TRUE) {

    # Check token
    if (is.na(.self$getPropertyValue('token')))
        .self$message('error', "Peakforest requires a token for this service.")

    # IDs are unique between LCMS and LCMSMS. Hence no confusion possible, and
    # ID used in LCMS is not used in LCMSMS.
    p <- c(token=.self$getPropertyValue('token'))
    bu <- c(.self$getPropValSlot('urls', 'ws.url'), 'spectra')
    if (concatenate) {
        ids <- paste(id, collapse=',')
        url <- c(BiodbUrl(url=c(bu, 'lcms', 'ids', ids), params=p)$toString(),
                 BiodbUrl(url=c(bu, 'lcmsms', 'ids', ids), params=p)$toString())
    }
    else {
        f1 <- function(x) BiodbUrl(url=c(bu, 'lcms', 'ids', x),
                                   params=p)$toString()
        f2 <- function(x) BiodbUrl(url=c(bu, 'lcmsms', 'ids', x),
                                   params=p)$toString()
        url <- c(vapply(id, f1, FUN.VALUE=''), vapply(id, f2, FUN.VALUE=''))
    }

    return(url)
}

))
