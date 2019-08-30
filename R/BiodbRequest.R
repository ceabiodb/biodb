# vi: fdm=marker ts=4 et cc=80 tw=80

# BiodbRequest {{{1
################################################################################

#' Class Request.
#'
#' This class represents a Request object that can be used with the Request
#' Scheduler.
#'
#' The constructor takes the following arguments:
#'
#' url: A \code{BiodbUrl} object.
#'
#' method: HTTP method. Either "get" or "post".
#'
#' header: The header.
#'
#' body: The body.
#'
#' encoding: The encoding to use.
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
    ":\n\nGets the URL.
    \nReturned value: The URL as a BiodbUrl object.
    "

    return(.self$.url)
},

# Get method {{{3
################################################################################

getMethod=function() {
    ":\n\nGets the method.
    \nReturned value: The method as a character value.
    "

    return(.self$.method)
},

# Get encoding {{{3
################################################################################

getEncoding=function() {
    ":\n\nGets the encoding. 
    \nReturned value: The encoding.
    "

    return(.self$.encoding)
},

# Get Curl options {{{3
################################################################################

getCurlOptions=function(useragent) {
    ":\n\nGets the options object to pass to cURL library.
    \nuseragent: The user agent as a character value.
    \nReturned value: An RCurl options object.
    "


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
    ":\n\nGets a unique key to identify this request. The key is an MD5 computed
    from the string representation of this request.
    \nReturned value: A unique key as an MD5 sum.
    "

    key <- digest::digest(.self$toString(), algo='md5')

    return(key)
},

# Get header as single string {{{3
################################################################################

getHeaderAsSingleString=function() {
    ":\n\nGets the HTTP header as a string, concatenating all its information
    into a single string.
    \nReturned value: The header as a single character value.
    "

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
    ":\n\nGets the body.
    \nReturned values: The body as a character value.
    "
    
    return(.self$.body)
},

# Show {{{3
################################################################################

show=function() {
    ":\n\nDisplays information about this instance.
    \nReturned value: None.
    "
    
    cat("Biodb request object on ", .self$.url$toString(), "\n", sep='')
},

# To string {{{3
################################################################################

toString=function() {
    ":\n\nGets a string representation of this instance.
    \nReturned value: A single string giving a representation of this instance.
    "

    request <- list(url=.self$.url$toString(), header=.self$.header,
                    body=.self$.body)
    request.json <- jsonlite::serializeJSON(request)
    request.json.str <- as.character(request.json)

    return(request.json.str)
}

))
