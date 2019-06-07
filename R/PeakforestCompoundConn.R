# vi: fdm=marker ts=4 et cc=80

# Class declaration {{{1
################################################################################

#' @include PeakforestConn.R
#' @include BiodbCompounddbConn.R
#' @include BiodbSearchable.R
PeakforestCompoundConn <- methods::setRefClass("PeakforestCompoundConn", contains=c("PeakforestConn", "BiodbCompounddbConn", 'BiodbSearchable'))

# Initialize {{{1
################################################################################

PeakforestCompoundConn$methods( initialize=function(...) {

    callSuper(db.name='compounds', ...)
})

# Get entry page url {{{1
################################################################################

PeakforestCompoundConn$methods( getEntryPageUrl=function(id) {
    return(vapply(id, function(x) BiodbUrl(url=.self$getPropValSlot('urls', 'base.url'), params=list(PFc=x))$toString(), FUN.VALUE=''))
})

# Get entry image url {{{1
################################################################################

PeakforestCompoundConn$methods( getEntryImageUrl=function(id) {
    return(rep(NA_character_, length(id)))
})

# Web service search.compounds.mass {{{1
################################################################################

PeakforestCompoundConn$methods( wsSearchCompoundsMass=function(field, mass, delta, max=NA_integer_, retfmt=c('plain', 'request', 'parsed', 'ids')) {

    retfmt <- match.arg(retfmt)

    # Check mass field
    if ( ! field %in% c('monoisotopicmass', 'averagemass'))
        .self$message('error', paste0('Unknown mass field "', field, '".'))

    # Build request
    params <- c(token=.self$getPropertyValue('token'))
    if ( ! is.na(max))
        params <- c(params, max=max)
    url <- BiodbUrl(url=c(.self$getPropValSlot('urls', 'ws.url'), 'search', 'compounds', field, mass, delta), params=params)
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

        # Extract IDs
        if (retfmt == 'ids') {
            if ('compounds' %in% names(results))
                results <- vapply(results$compounds, function(x) as.character(x$id), FUN.VALUE='')
            else
                .self$message('error', 'Could find "compounds" field inside returned JSON.')
        }
    }

    return(results)
})

# Search by name {{{1
################################################################################

PeakforestCompoundConn$methods( searchByName=function(name, max.results=NA_integer_) {
    return(.self$searchCompound(name=name, max.results=max.results))
})

# Search compound {{{1
################################################################################

PeakforestCompoundConn$methods( searchCompound=function(name=NULL, mass=NULL, mass.field=NULL, mass.tol=0.01, mass.tol.unit='plain', max.results=NA_integer_) {
        
    .self$.checkMassField(mass=mass, mass.field=mass.field)

    ids <- NULL

    # Will we search for mass
    search.mass <- FALSE
    if ( ! is.null(mass.field) && ! is.null(mass)) {
        if (mass.field %in% c('monoisotopic.mass', 'average.mass'))
            search.mass <- TRUE
        else
            .self$message('caution', paste0('Mass field ', mass.field, ' is not handled.'))
    }

    # Search by name
    if ( ! is.null(name)) {
        max <- if (search.mass) NA_integer_ else max.results
        ids <- .self$wsSearch(name, max=max, retfmt='ids')
    }

    # Search by mass
    if (search.mass) {
        if (mass.tol.unit == 'ppm')
            delta <- mass * mass.tol * 1e-6
        else
            delta <- mass.tol
        field <- if (mass.field == 'monoisotopic.mass') 'monoisotopicmass' else 'averagemass'
        max <- if (is.null(name)) max.results else NA_integer_
        mass.ids <- .self$wsSearchCompoundsMass(field=field, mass=mass, delta=delta, max=max, retfmt='ids')
        if ( ! is.null(ids))
            ids <- ids[ids %in% mass.ids]
        else
            ids <- mass.ids
    }

    # Cut
    if ( ! is.na(max.results) && length(ids) > max.results)
        ids <- ids[seq_len(max.results)]

    return(ids)
})

# Private methods {{{1
################################################################################

# Get entry content request {{{2
################################################################################

PeakforestCompoundConn$methods( .doGetEntryContentRequest=function(id, concatenate=TRUE) {

    # Check token
    if (is.na(.self$getPropertyValue('token')))
        .self$message('error', "Peakforest requires a token for this service.")

    return(vapply(id, function(x) BiodbUrl(url=c(.self$getPropValSlot('urls', 'ws.url'), 'compounds', x), params=list(token=.self$getPropertyValue('token')))$toString(), FUN.VALUE=''))
})
