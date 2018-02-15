# vi: fdm=marker

# Class declaration {{{1
################################################################

#' @include RemotedbConn.R
PeakforestConn <- methods::setRefClass("PeakforestConn", contains = c("RemotedbConn"), fields = list(.db.name = 'character'))

# Constructor {{{1
################################################################

PeakforestConn$methods( initialize = function(db.name, ...) {

	# Call mother class constructor
	callSuper(...)
	.self$.abstract.class('PeakforestConn')

	# Set db name
	.db.name <<- db.name

	# Check token
	if (is.na(.self$getToken()))
		.self$message('caution', "Peakforest requires a token to function correctly.")
})

# Check if error {{{1
################################################################

PeakforestConn$methods( .checkIfError = function(content) {

	if (length(grep('^<!DOCTYPE HTML ', content)) > 0) {
		.self$message('debug', paste("Peakforest returned error: ", content))
		return(TRUE)
	}

	if (length(grep('^<html>.*Apache Tomcat.*Error report', content)) > 0)
		.self$message('error', paste("Peakforest connection error: ", content))

	return(FALSE)
})

# Get entry content {{{1
################################################################

PeakforestConn$methods( getEntryContent = function(entry.id) {
	
	# Initialize contents to return
	content <- rep(NA_character_, length(entry.id))

	# Get URLs
	urls <- .self$getEntryContentUrl(entry.id, max.length = 2048)

	# Send request
	jsonstr <- vapply(urls, function(url) .self$.getUrlScheduler()$getUrl(url), FUN.VALUE = '')

	# Get directly one JSON string for each ID
	if (length(jsonstr) == length(entry.id)) {
		for (i in seq_along(jsonstr)) {
			json = jsonlite::fromJSON(jsonstr[[i]], simplifyDataFrame = FALSE)
			if (is.null(json))
				next
			if (class(json) == 'list' && ! is.null(names(json))) {
			content <- jsonstr
			return(content)
			}
			else
				break
		}
	}

	# Parse all JSON strings
	for (single.jsonstr in jsonstr) {

		if (.self$.checkIfError(single.jsonstr))
			break

		json <- jsonlite::fromJSON(single.jsonstr, simplifyDataFrame = FALSE)

		if ( ! is.null(json)) {
			if (class(json) == 'list' && is.null(names(json))) {
				null <- vapply(json, is.null, FUN.VALUE = TRUE)
				json.ids <- vapply(json[ ! null], function(x) as.character(x$id), FUN.VALUE = '')
				content[entry.id %in% json.ids] <- vapply(json[ ! null], function(x) jsonlite::toJSON(x, pretty = TRUE, digits = NA_integer_), FUN.VALUE = '')
			}
		}
	}

	return(content)
})

# Get entry ids {{{1
################################################################

PeakforestConn$methods( getEntryIds = function(max.results = NA_integer_) {

	# Get all IDs
	ids <- .self$ws.all.ids(biodb.ids = TRUE)

	# Cut
	if ( ! is.na(max.results) && max.results > 1 && max.results < length(ids))
		ids <- ids[1:max.results]

	return(ids)
})

# Get nb entries {{{1
################################################################

PeakforestConn$methods( getNbEntries = function(count = FALSE) {
	return(.self$ws.all.count(biodb.parse = TRUE))
})

# Web service search {{{1
################################################################

PeakforestConn$methods( ws.search = function(term, max = NA_integer_, biodb.parse = FALSE, biodb.ids = FALSE) {

	# Build URL
	url <- paste(.self$getBaseUrl(), 'search/', .self$.db.name, '/', term, sep = '')
	params <- c(token = .self$getToken())
	if ( ! is.na(max))
		params <- c(params, max = max)

	# Send request
	results <- .self$.getUrlScheduler()$getUrl(url, params = params)

	# Parse results
	if (biodb.parse || biodb.ids)
		results <- jsonlite::fromJSON(results, simplifyDataFrame = FALSE)

	# Extract IDs
	if (biodb.ids) {
		if ('compoundNames' %in% names(results))
			results <- vapply(results$compoundNames, function(x) as.character(x$compound$id), FUN.VALUE = '')
		else
			.self$message('error', 'Could find "compoundNames" field inside returned JSON.')
	}

	return(results)
})

# Web service all.count {{{1
################################################################

PeakforestConn$methods( ws.all.count = function(biodb.parse = FALSE) {

	# Build URL
	url <- paste(.self$getBaseUrl(), .self$.db.name, '/', 'all/count', sep = '')
	params <- c(token = .self$getToken())

	# Send request
	results <- .self$.getUrlScheduler()$getUrl(url, params = params)

	# Parse integer
	if (biodb.parse)
		results <- as.integer(results)

	return(results)
})

# Web service all.ids {{{1

################################################################

PeakforestConn$methods( ws.all.ids = function(biodb.parse = FALSE,  biodb.ids = FALSE) {

	# Build URL
	url <- paste(.self$getBaseUrl(), .self$.db.name, '/', 'all/ids', sep = '')
	params <- c(token = .self$getToken())

	# Send request
	results <- .self$.getUrlScheduler()$getUrl(url, params = params)
	.self$.checkIfError(results)

	# Parse JSON
	if (biodb.parse || biodb.ids)
		results <- jsonlite::fromJSON(results, simplifyDataFrame = FALSE)

	# extract IDs
	if (biodb.ids)
		results <- as.character(results)

	return(results)
})
