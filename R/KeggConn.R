# vi: fdm=marker

# Class declaration {{{1
################################################################

#'The mother abstract class of all KEGG connection classes.
#'@export
KeggConn <- methods::setRefClass("KeggConn", contains = "RemotedbConn", fields = list(.db.name = "character", .db.abbrev = "character"))

# Constructor {{{1
################################################################

KeggConn$methods( initialize = function(db.name = NA_character_, db.abbrev = NA_character_, ...) {

	callSuper(base.url = 'http://rest.kegg.jp/', ...)

	# Set name
	if (is.null(db.name) || is.na(db.name))
		.self$message(MSG.ERROR, "You must set a name this KEGG database.")
	.db.name <<- db.name

	# Set abbreviation
	if (is.null(db.abbrev) || is.na(db.abbrev))
		.self$message(MSG.ERROR, "You must set a abbreviation this KEGG database.")
	.db.abbrev <<- db.abbrev
})

# Complete entry id {{{1
################################################################

KeggConn$methods( .complete.entry.id = function(id) {
	return(paste(.self$.db.abbrev, id, sep = ':'))
})

# Get entry content url {{{1
################################################################

KeggConn$methods( getEntryContentUrl = function(id) {
	return(paste(.self$.base.url, 'get/', .self$.complete.entry.id(id), sep = ''))
})

# Get entry page url {{{1
################################################################

KeggConn$methods( getEntryPageUrl = function(id) {
	return(paste('http://www.genome.jp/dbget-bin/www_bget?', .self$.complete.entry.id(id), sep = ''))
})

# Get entry content type {{{1
################################################################

KeggConn$methods( getEntryContentType = function() {
	return(BIODB.TXT)
})

# Get entry content {{{1
################################################################

KeggConn$methods( getEntryContent = function(id) {

	# Initialize return values
	content <- rep(NA_character_, length(id))

	# Request
	content <- vapply(id, function(x) .self$.get.url(getEntryContentUrl(x)), FUN.VALUE = '')

	return(content)
})

# Get nb entries {{{1
################################################################

KeggConn$methods( getNbEntries = function(count = FALSE) {
	return(if (count) length(.self$getEntryIds()) else NA_integer_)
})

# Get entry ids {{{1
################################################################

KeggConn$methods( getEntryIds = function(max.results = NA_integer_) {

	# Get IDs
	ids <- .self$.get.url(paste(.self$.base.url, 'list/', .self$.db.name, sep = ''))

	# Extract IDs
	ids <- strsplit(ids, "\n")[[1]]
	#ids <- sub('^[^:]+:([^\\s]+)\\s.*$', '\\1', ids, perl = TRUE)
	ids <- sub('^[^:]+:([^\\s]+)\\s.*$', '\\1', ids, perl = TRUE)

	# Cut results
	if ( ! is.na(max.results) && max.results > 0)
		ids <- ids[1:max.results]

	return(ids)
})
