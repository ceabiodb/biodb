# vi: fdm=marker

#' @include RemotedbConn.R

# Class declaration {{{1
################################################################

#'PeakForest connector class.
#'@export
PeakforestConn <- methods::setRefClass("PeakforestConn", contains = c("RemotedbConn"), fields = list(.db.name = 'character'))

# Constructor {{{1
################################################################

PeakforestConn$methods( initialize = function(db.name, ...) {

	callSuper(content.type = BIODB.JSON, base.url = .self$getBiodb()$getConfig()$get(CFG.PEAKFOREST.URL), token = .self$getBiodb()$getConfig()$get(CFG.PEAKFOREST.TOKEN), ...)

	# Set db name
	.db.name <<- db.name

	# Check token
	if (is.na(.self$getToken()))
		.self$message(MSG.CAUTION, "Peakforest requires a token to function correctly.")
})

# Get entry content {{{1
################################################################

PeakforestConn$methods( getEntryContent = function(id) {
	
	# Initialize contents to return
	content <- rep(NA_character_, length(id))

	# Get URLs
	urls <- .self$getEntryContentUrl(id)

	# Send request
	jsonstr <- vapply(urls, function(url) .self$.getUrlScheduler()$getUrl(url), FUN.VALUE = '')

	# Parse JSON
	if (length(jsonstr) > 1) {
		if (length(jsonstr) != length(id))
			.self$message(MSG.ERROR, paste("Got only", length(jsonstr), "contents for", length(id), "IDs."))
		content <- jsonstr
	}
	else {
		json <- jsonlite::fromJSON(jsonstr, simplifyDataFrame = FALSE)

		if ( ! is.null(json)) {
			if (length(json) > 1) {
				json.ids <- vapply(json, function(x) as.character(x$id), FUN.VALUE = '')
				content[id %in% json.ids] <- vapply(json, function(x) jsonlite::toJSON(x, pretty = TRUE, digits = NA_integer_), FUN.VALUE = '')
			}
			else if (length(json) == 1)
				content <- json
		}
	}

	return(content)
})

# Get entry ids {{{1
################################################################

PeakforestConn$methods( getEntryIds = function(max.results = NA_integer_) {

	# Build URL
	url <- paste(.self$getBaseUrl(), .self$.db.name, '/all/ids?token=', .self$getToken(), sep = '')

	# Send request
	json.str <- .self$.getUrlScheduler()$getUrl(url)

	# Parse JSON
	json <- jsonlite::fromJSON(json.str, simplifyDataFrame = FALSE)

	# Get IDs
	ids <- json

	# Cut
	if ( ! is.na(max.results) && max.results > 1 && max.results < length(ids))
		ids <- ids[1:max.results]

	return(ids)
})

# Get nb entries {{{1
################################################################

PeakforestConn$methods( getNbEntries = function(count = FALSE) {

	# Build URL
	url <- paste(.self$getBaseUrl(), .self$.db.name, '/all/count?token=', .self$getToken(), sep = '')

	# Send request
	n <- as.integer(.self$.getUrlScheduler()$getUrl(url))

	return(n)
})
