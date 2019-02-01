# vi: fdm=marker

# Class declaration {{{1
################################################################

#' Class URL.
#'
#' This class represents a URL object that can be used in requests.
#'
#' @param url           The URL to access, as a character string.
#' @param params        The list of parameters to use with the URL.
#'
#' @seealso \code{\link{BiodbRequestScheduler}}, \code{\link{BiodbRequest}}.
#'
#' @import methods
#' @export BiodbUrl
#' @exportClass BiodbUrl
BiodbUrl <- methods::setRefClass("BiodbUrl", fields = list(.url = 'character', .params = 'character'))

# Constructor {{{1
################################################################

BiodbUrl$methods( initialize = function(url = character(), params = character()) {

	.url <<- url
	.params <<- if (is.character(params)) params else as.character(unlist(params))
})

# Get domain {{{1
################################################################

BiodbUrl$methods( getDomain = function() {

	domain <- sub('^.+://([^/]+)(/.*)?$', '\\1', .self$.url, perl = TRUE)

	return(domain)
})

# Set URL {{{1
################################################################

BiodbUrl$methods( setUrl = function(url) {
	.url <<- url
})

# Set parameter {{{1
################################################################

BiodbUrl$methods( setParam = function(key, value) {
	.self$.params[[key]] <- value
})

# To string {{{1
################################################################

BiodbUrl$methods( toString = function(encode = TRUE) {

	url <- .self$.url

	# Add parameters to URL
	if (length(.self$.params) > 0) {

		# Build parameters string
		pn <- names(.self$.params)
		kv.list <- vapply(seq_along(.self$.params), function(i) if (is.null(pn) || nchar(pn[[i]]) == 0) .self$.params[[i]] else paste(pn[[i]], .self$.params[[i]], sep = '='), FUN.VALUE = '')
		params.str <- paste(kv.list, collapse = '&')

		# Concatenate URL with parameters
		url <- paste(.self$.url, params.str, sep = '?')
	}

	if (encode)
		url <- utils::URLencode(url)

	return(url)
})
