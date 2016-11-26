if ( ! exists('NcbigeneConn')) { # Do not load again if already loaded

	#####################
	# CLASS DECLARATION #
	#####################

	NcbigeneConn <- setRefClass("NcbigeneConn", contains = "RemotedbConn")

	###############
	# CONSTRUCTOR #
	###############

	NcbigeneConn$methods( initialize = function(...) {
		# From NCBI E-Utility manual: "In order not to overload the E-utility servers, NCBI recommends that users post no more than three URL requests per second and limit large jobs to either weekends or between 9:00 PM and 5:00 AM Eastern time during weekdays".
		callSuper(scheduler = UrlRequestScheduler$new(n = 3), ...)
	})

	##########################
	# GET ENTRY CONTENT TYPE #
	##########################

	NcbigeneConn$methods( getEntryContentType = function() {
		return(BIODB.XML)
	})

	#####################
	# GET ENTRY CONTENT #
	#####################
	
	NcbigeneConn$methods( getEntryContent = function(id) {

		# Initialize return values
		content <- rep(NA_character_, length(id))

		# Request
		content <- vapply(id, function(x) .self$.get.url(get.entry.url(BIODB.NCBIGENE, x, content.type = BIODB.XML)), FUN.VALUE = '')

		return(content)
	})
	
	################
	# CREATE ENTRY #
	################
	
	NcbigeneConn$methods( createEntry = function(content, drop = TRUE) {
		return(createNcbigeneEntryFromXml(content, drop = drop))
	})
}
