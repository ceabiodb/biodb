# vi: fdm=marker

# Class declaration {{{1
################################################################

LipidmapsStructureConn <- methods::setRefClass("LipidmapsStructureConn", contains = "RemotedbConn")

# Constructor {{{1
################################################################

LipidmapsStructureConn$methods( initialize = function(...) {
	# If you write a script to automate calls to LMSD, please be kind and do not hit our server more often than once per 20 seconds. We may have to kill scripts that hit our server more frequently.
	callSuper(base.url = 'http://www.lipidmaps.org/data/', ...)
})

# Get entry content url {{{1
################################################################

LipidmapsStructureConn$methods( .doGetEntryContentUrl = function(id, concatenate = TRUE) {
	return(paste(.self$getBaseUrl(), 'LMSDRecord.php', '?Mode=File&LMID=', id, '&OutputType=CSV&OutputQuote=No', sep = ''))
})

# Get entry page url {{{1
################################################################

LipidmapsStructureConn$methods( getEntryPageUrl = function(id) {
	return(paste(.self$getBaseUrl(), '?LMID=', id, sep = ''))
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
