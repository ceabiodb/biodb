#####################
# CLASS DECLARATION #
#####################

NcbiccdsConn <- methods::setRefClass("NcbiccdsConn", contains = "RemotedbConn")

###############
# CONSTRUCTOR #
###############

NcbiccdsConn$methods( initialize = function(...) {
	# From NCBI E-Utility manual: "In order not to overload the E-utility servers, NCBI recommends that users post no more than three URL requests per second and limit large jobs to either weekends or between 9:00 PM and 5:00 AM Eastern time during weekdays".
	callSuper(scheduler = UrlRequestScheduler$new(n = 3, parent = .self), ...)
})

##########################
# GET ENTRY CONTENT TYPE #
##########################

NcbiccdsConn$methods( getEntryContentType = function() {
	return(BIODB.HTML)
})

#####################
# GET ENTRY CONTENT #
#####################

NcbiccdsConn$methods( getEntryContent = function(id) {

	# Initialize return values
	content <- rep(NA_character_, length(id))

	# Request
	content <- vapply(id, function(x) .self$.get.url(get.entry.url(BIODB.NCBICCDS, x, content.type = BIODB.HTML)), FUN.VALUE = '')

	return(content)
})

################
# CREATE ENTRY #
################

NcbiccdsConn$methods( createEntry = function(content, drop = TRUE) {
	return(createNcbiccdsEntryFromHtml(.self$getBiodb(), content, drop = drop))
})
