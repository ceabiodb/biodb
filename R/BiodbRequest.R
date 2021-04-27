#' Class Request.
#'
#' This class represents a Request object that can be used with the Request
#' Scheduler.
#'
#' @seealso \code{\link{BiodbRequestScheduler}}, \code{\link{BiodbUrl}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::newInst()
#'
#' # Create a request object
#' u <- 'https://www.ebi.ac.uk/webservices/chebi/2.0/test/getCompleteEntity'
#' url <- BiodbUrl$new(url=u)
#' url$setParam('chebiId', 15440)
#' request <- BiodbRequest$new(method='get', url=url)
#'
#' # Send request
#' mybiodb$getRequestScheduler()$sendRequest(request)
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @import R6
#' @import openssl
#' @include BiodbUrl.R
#' @export
BiodbRequest <- R6::R6Class("BiodbRequest",

public=list(

#' @description
#' Constructor.
#' @param url A \code{BiodbUrl} object.
#' @param method HTTP method. Either "get" or "post".
#' @param header The header.
#' @param body The body.
#' @param encoding The encoding to use.
#' @param conn A valid BiodbConn instance for which this request is built.
#' @return A new instance.
initialize=function(url, method=c('get', 'post'), header=character(),
                    body=character(), encoding=integer(), conn=NULL) {

    private$url <- url
    private$method <- match.arg(method)
    private$header <- header
    private$body <- body
    private$encoding <- encoding
    private$conn <- NULL
},

#' @description
#' Sets the associated connector (usually the connector that created this
#' request).
#' @param conn A valid BiodbConn object.
#' @return None.
setConn=function(conn) {

    if ( ! methods::is(conn, 'BiodbConn'))
        stop("Parameter conn must be a BiodbConn object.")

    private$conn <- conn

    invisible(NULL)
},

#' @description
#' gets the associated connector (usually the connector that created this
#' request).
#' @return The associated connector as a BiodbConn object.
getConn=function() {

    return(private$conn)
},

#' @description
#' Gets the URL.
#' @return The URL as a BiodbUrl object.
getUrl=function() {

    return(private$url)
},

#' @description
#' Gets the method.
#' @return The method as a character value.
getMethod=function() {

    return(private$method)
},

#' @description
#' Gets the encoding. 
#' @return The encoding.
getEncoding=function() {

    return(private$encoding)
},

#' @description
#' Gets the options object to pass to cURL library.
#' @param useragent The user agent as a character value.
#' @return An RCurl options object.
getCurlOptions=function(useragent) {


    opts <- list()
    if (length(private$header) > 0)
        opts$httpheader <- private$header
    if (length(private$body) > 0)
        opts$postfields <- private$body

    opts <- RCurl::curlOptions(useragent=useragent, timeout.ms=60000,
                               verbose=FALSE, .opts=opts)

    return(opts)
},

#' @description
#' Gets a unique key to identify this request. The key is an MD5 sum
#' computed from the string representation of this request.
#' @return A unique key as an MD5 sum.
getUniqueKey=function() {

    key <- openssl::md5(self$toString())

    return(key)
},

#' @description
#' Gets the HTTP header as a string, concatenating all its information
#' into a single string.
#' @return The header as a single character value.
getHeaderAsSingleString=function() {

    s <- ''

    if (length(private$header) > 0) {
        fct <- function(k) paste(k, private$header[[k]], sep='=')
        kv <- vapply(names(private$header), fct, FUN.VALUE='')
        s <- paste(kv, collapse=', ')
    }

    return(s)
},

#' @description
#' Gets the body.
#' @return The body as a character value.
getBody=function() {

    return(private$body)
},

#' @description
#' Displays information about this instance.
#' @return None.
print=function() {

    cat("Biodb request object on ", private$url$toString(), "\n", sep='')
 
    return(invisible(self))
},

#' @description
#' Gets a string representation of this instance.
#' @return A single string giving a representation of this instance.
toString=function() {

    request <- list(url=private$url$toString(), header=private$header,
                    body=private$body)
    request.json <- jsonlite::serializeJSON(request)
    request.json.str <- as.character(request.json)

    return(request.json.str)
}
),

private=list(
    url=NULL,
    method=NULL,
    header=NULL,
    body=NULL,
    encoding=NULL,
    conn=NULL
))
