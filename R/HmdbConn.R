#####################
# CLASS DECLARATION #
#####################

HmdbConn <- methods::setRefClass("HmdbConn", contains = "RemotedbConn")

##########################
# GET ENTRY CONTENT TYPE #
##########################

HmdbConn$methods( getEntryContentType = function() {
	return(BIODB.XML)
})

#####################
# GET ENTRY CONTENT #
#####################

HmdbConn$methods( getEntryContent = function(id) {

	# Initialize return values
	content <- rep(NA_character_, length(id))

	# Request
	content <- vapply(id, function(x) .self$.get.url(get.entry.url(BIODB.HMDB, x, content.type = BIODB.XML)), FUN.VALUE = '')

	return(content)
})

################
# CREATE ENTRY #
################

HmdbConn$methods( createEntry = function(content, drop = TRUE) {
	return(createHmdbEntryFromXml(.self$getBiodb(), content, drop = drop))
})

# Get entry ids {{{1
################################################################

HmdbConn$methods( getEntryIds = function(max.results = NA_integer_) {

	ids <- NULL

	# TODO Needs first to create BiodbConfig and BiodbCache classes.
	# TODO Rename HmdbConn to HmdbmetaboliteConn

	# Do we allow database download? This can take some time.
	if (.self$getBiodb()$getConfig()$enabled(CFG.DBDWNLD)) {

		# Download entries if not already done. --> downloadzip http://www.hmdb.ca/system/downloads/current/hmdb_metabolites.zip
		# Expand zip, and copy files into cache folder (remove old files first).
		# TODO How to know it has not already been downloaded?
		# List files to get all entry IDs
		# Count files to get number of entries
	}

	return(ids)
})
