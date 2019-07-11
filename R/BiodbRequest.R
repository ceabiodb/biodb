# vi: fdm=marker ts=4 et cc=80 tw=80

# BiodbRequest {{{1
################################################################################

#' Class Request.
#'
#' This class represents a Request object that can be used with the Request
#' Scheduler.
#'
#' @param url           A \code{BiodbUrl} object.
#'
#' @seealso \code{\link{BiodbRequestScheduler}}, \code{\link{BiodbUrl}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Create a request object
#' u <- 'https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/1/XML'
#' url <- BiodbUrl(url=u)
#' request <- BiodbRequest(method='get', url=url)
#'
#' # Send request
#' \dontrun{
#' mybiodb$getRequestScheduler()$sendRequest(request)
#' }
#'
#' # Terminate instance.
#' mybiodb$terminate()
#' mybiodb <- NULL
#'
#' @import methods
#' @include BiodbUrl.R
#' @export BiodbRequest
#' @exportClass BiodbRequest
BiodbRequest <- methods::setRefClass("BiodbRequest",
    fields=list(
        .url='BiodbUrl',
        .method='character',
        .header='character',
        .body='character',
        .encoding='ANY'
    ),

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(url, method=c('get', 'post'), header=character(),
                    body=character(), encoding=integer()) {

    .self$.url <- url
    .self$.method <- match.arg(method)
    .self$.header <- header
    .self$.body <- body
    .self$.encoding <- encoding
},

# Get URL {{{3
################################################################################

getUrl=function() {
    return(.self$.url)
},

# Get method {{{3
################################################################################

getMethod=function() {
    return(.self$.method)
},

# Get encoding {{{3
################################################################################

getEncoding=function() {
    return(.self$.encoding)
},

# Get Curl options {{{3
################################################################################

getCurlOptions=function(useragent) {

    opts <- list()
    if (length(.self$.header) > 0)
        opts$httpheader <- .self$.header
    if (length(.self$.body) > 0)
        opts$postfields <- .self$.body

    opts <- RCurl::curlOptions(useragent=useragent, timeout.ms=60000,
                               verbose=FALSE, .opts=opts)

    return(opts)
},

# Get unique key {{{3
################################################################################

getUniqueKey=function() {

    key <- digest::digest(.self$toString(), algo='md5')

    return(key)
},

# Get header as single string {{{3
################################################################################

getHeaderAsSingleString=function() {

    s <- ''

    if (length(.self$.header) > 0) {
        fct <- function(k) paste(k, .self$.header[[k]], sep='=')
        kv <- vapply(names(.self$.header), fct, FUN.VALUE='')
        s <- paste(kv, collapse=', ')
    }

    return(s)
},

# Get body {{{3
################################################################################

getBody=function() {
    return(.self$.body)
},

# Show {{{3
################################################################################

show=function() {
    cat("Biodb request object on ", .self$.url$toString(), "\n", sep='')
},

# To string {{{3
################################################################################

toString=function() {

    request <- list(url=.self$.url$toString(), header=.self$.header,
                    body=.self$.body)
    request.json <- jsonlite::serializeJSON(request)
    request.json.str <- as.character(request.json)

    return(request.json.str)
}

))
