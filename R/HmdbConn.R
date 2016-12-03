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
	return(createHmdbEntryFromXml(content, drop = drop))
})
