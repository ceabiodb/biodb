# vi: fdm=marker

# Class declaration {{{1
################################################################

#' Class Request.
#'
#' This class represents a Request object that can be used with the Request Scheduler.
#'
#' @param url           A \code{BiodbUrl} object.
#'
#' @seealso \code{\link{BiodbRequestScheduler}}, \code{\link{BiodbUrl}}.
#'
#' @import methods
#' @export BiodbRequest
#' @exportClass BiodbRequest
BiodbRequest <- methods::setRefClass("BiodbRequest", fields = list(.url = 'BiodbUrl', .method = 'character', .header = 'character', .body = 'character', .encoding = 'ANY'))

# Constructor {{{1
################################################################

BiodbRequest$methods( initialize = function(url, method = c('get', 'post'), header = character(), body = character(), encoding = integer()) {

	.url <<- url
	.method <<- match.arg(method)
	.header <<- header
	.body <<- body
	.encoding <<- encoding
})

# Get URL {{{1
################################################################

BiodbRequest$methods( getUrl = function() {
	return(.self$.url)
})

# Get method {{{1
################################################################

BiodbRequest$methods( getMethod = function() {
	return(.self$.method)
})

# Get encoding {{{1
################################################################

BiodbRequest$methods( getEncoding = function() {
	return(.self$.encoding)
})

# Get Curl options {{{1
################################################################

BiodbRequest$methods( getCurlOptions = function(useragent) {

	opts <- list()
	if (length(.self$.header) > 0)
		opts$httpheader <- .self$.header
	if (length(.self$.body) > 0)
		opts$postfields <- .self$.body

	opts <- RCurl::curlOptions(useragent = useragent, timeout.ms = 60000, verbose = FALSE, .opts = opts)

	return(opts)
})

# Get unique key {{{1
################################################################

BiodbRequest$methods( getUniqueKey = function() {

	key <- digest::digest(.self$toString(), algo = 'md5')

	return(key)
})

# Get header as single string {{{1
################################################################

BiodbRequest$methods( getHeaderAsSingleString = function() {

	s <- ''

	if (length(.self$.header) > 0)
		s <- paste(vapply(names(.self$.header), function(k) paste(k, .self$.header[[k]], sep = '='), FUN.VALUE = ''), collapse = ', ')

	return(s)
})

# Get body {{{1
################################################################

BiodbRequest$methods( getBody = function() {
	return(.self$.body)
})

# To string {{{1
################################################################

BiodbRequest$methods( toString = function() {

	request <- list(url = .self$.url$toString(), header = .self$.header, body = .self$.body)
	request.json <- jsonlite::serializeJSON(request)
	request.json.str <- as.character(request.json)

	return(request.json.str)
})
