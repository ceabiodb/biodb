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
#' u <- 'https://www.ebi.ac.uk/webservices/chebi/2.0/test/getCompleteEntity'
#' url <- BiodbUrl$new(url=u)
#' url$setParam('chebiId', 15440)
#' request <- BiodbRequest(method='get', url=url)
#'
#' # Send request
#' mybiodb$getRequestScheduler()$sendRequest(request)
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @import methods
#' @import openssl
#' @include BiodbUrl.R
#' @export BiodbRequest
#' @exportClass BiodbRequest
BiodbRequest <- methods::setRefClass("BiodbRequest",
    fields=list(
        .url='ANY',
        .method='character',
        .header='character',
        .body='character',
        .encoding='ANY',
        conn='ANY'
    ),

methods=list(

initialize=function(url, method=c('get', 'post'), header=character(),
                    body=character(), encoding=integer(), conn=NULL) {

    .self$.url <- url
    .self$.method <- match.arg(method)
    .self$.header <- header
    .self$.body <- body
    .self$.encoding <- encoding
    .self$conn <- NULL
},

setConn=function(conn) {
    ":\n\nSets the associated connector (usually the connector that created this
    request).
    \nconn: A valid BiodbConn object.
    \nReturned value: None.
    "

    if ( ! methods::is(conn, 'BiodbConn'))
        stop("Parameter conn must be a BiodbConn object.")

    .self$conn <- conn

    invisible(NULL)
},

getConn=function() {
    ":\n\ngets the associated connector (usually the connector that created this
    request).
    \nReturned value: The associated connector as a BiodbConn object.
    "

    return(.self$conn)
},

getUrl=function() {
    ":\n\nGets the URL.
    \nReturned value: The URL as a BiodbUrl object.
    "

    return(.self$.url)
},

getMethod=function() {
    ":\n\nGets the method.
    \nReturned value: The method as a character value.
    "

    return(.self$.method)
},

getEncoding=function() {
    ":\n\nGets the encoding. 
    \nReturned value: The encoding.
    "

    return(.self$.encoding)
},

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

getUniqueKey=function() {
    ":\n\nGets a unique key to identify this request. The key is an MD5 sum
    computed from the string representation of this request.
    \nReturned value: A unique key as an MD5 sum.
    "

    key <- openssl::md5(.self$toString())

    return(key)
},

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

getBody=function() {
    ":\n\nGets the body.
    \nReturned values: The body as a character value.
    "

    return(.self$.body)
},

show=function() {
    ":\n\nDisplays information about this instance.
    \nReturned value: None.
    "

    cat("Biodb request object on ", .self$.url$toString(), "\n", sep='')
},

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
