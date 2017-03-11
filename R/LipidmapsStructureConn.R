# vi: fdm=marker

# Class declaration {{{1
################################################################

LipidmapsStructureConn <- methods::setRefClass("LipidmapsstructureConn", contains = "RemotedbConn")

# Constructor {{{1
################################################################

LipidmapsStructureConn$methods( initialize = function(...) {
	# From http://www.lipidmaps.org/data/structure/programmaticaccess.html:
	# If you write a script to automate calls to LMSD, please be kind and do not hit our server more often than once per 20 seconds. We may have to kill scripts that hit our server more frequently.
	callSuper(content.type = BIODB.CSV, base.url = 'http://www.lipidmaps.org/data/', scheduler = UrlRequestScheduler$new(t = 20, parent = .self), ...)
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

LipidmapsStructureConn$methods( getEntryContent = function(id) {

	# Initialize return values
	content <- rep(NA_character_, length(id))

	# Request
	content <- vapply(id, function(x) .self$.getUrlScheduler()$getUrl(.self$getEntryContentUrl(x)), FUN.VALUE = '')

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
