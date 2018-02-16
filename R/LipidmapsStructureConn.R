# vi: fdm=marker

# Class declaration {{{1
################################################################

#' @include CompounddbConn.R
#' @include RemotedbConn.R
LipidmapsStructureConn <- methods::setRefClass("LipidmapsStructureConn", contains = c("RemotedbConn", "CompounddbConn"))

# Get entry content url {{{1
################################################################

LipidmapsStructureConn$methods( .doGetEntryContentUrl = function(ids, concatenate = TRUE) {
	return(vapply(ids, function(id) .self$ws.LMSDRecord(mode = 'File', lmid = id, biodb.url = TRUE), FUN.VALUE = ''))
})

# Get entry page url {{{1
################################################################

LipidmapsStructureConn$methods( getEntryPageUrl = function(id) {
	return(paste(.self$getBaseUrl(), '?LMID=', id, sep = ''))
})

# Get entry image url {{{1
################################################################

LipidmapsStructureConn$methods( getEntryImageUrl = function(id) {
	return(rep(NA_character_, length(id)))
})

# Get entry content {{{1
################################################################

LipidmapsStructureConn$methods( getEntryContent = function(entry.id) {

	# Initialize return values
	content <- rep(NA_character_, length(entry.id))

	# Request
	content <- vapply(entry.id, function(x) .self$.getUrlScheduler()$getUrl(.self$getEntryContentUrl(x)), FUN.VALUE = '')

	return(content)
})

# Get entry ids {{{1
################################################################

LipidmapsStructureConn$methods( getEntryIds = function(max.results = NA_integer_) {

	# Retrieve all entries
	result.txt <- .self$.getUrlScheduler()$getUrl(paste(.self$getBaseUrl(), 'structure/LMSDSearch.php?Mode=ProcessStrSearch&OutputMode=File', sep = ''))

	# Convert into data frame
	result.df <- read.table(text = result.txt, sep = "\t", header = TRUE, comment.char = '', stringsAsFactors = FALSE, quote = '')

	# Extract IDs
	ids <- result.df[['LM_ID']]

	# Cut
	if ( ! is.na(max.results) && length(ids) > max.results)
		ids <- ids[1:max.results]

	return(ids)
})

# Web service LMSDRecord {{{1
################################################################

LipidmapsStructureConn$methods( ws.LMSDRecord = function(mode, lmid, output.type = 'CSV', output.quote = 'No', biodb.url = FALSE) {

	# Build request
	url <- paste0(.self$getBaseUrl(), 'LMSDRecord.php')
	params <- c(Mode = mode, LMID = lmid, OutputType = output.type, OutputQuote = output.quote)

	# Returns URL
	if (biodb.url)
		return(.self$.getUrlScheduler()$getUrlString(url, params))

	# Send request
	results <- .self$.getUrlScheduler()$getUrl(url, params)

	return(results)
})
