if ( ! exists('NcbigeneConn')) { # Do not load again if already loaded

	source('BiodbConn.R')
	source('NcbigeneCompound.R')

	#####################
	# CLASS DECLARATION #
	#####################

	NcbigeneConn <- setRefClass("NcbigeneConn", contains = "BiodbConn")

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

	NcbigeneConn$methods( getEntryContentType = function(type) {
		return(RBIODB.XML)
	})

	#####################
	# GET ENTRY CONTENT #
	#####################
	
	NcbigeneConn$methods( getEntryContent = function(type, id) {

		if (type == RBIODB.COMPOUND) {

			# Initialize return values
			content <- rep(NA_character_, length(id))

			# Request
			content <- vapply(id, function(x) .self$.scheduler$getUrl(get.entry.url(RBIODB.NCBIGENE, x)), FUN.VALUE = '')

			return(content)
		}

		return(NULL)
	})
	
	################
	# CREATE ENTRY #
	################
	
	NcbigeneConn$methods( createEntry = function(type, content, drop = TRUE) {
		return(if (type == RBIODB.COMPOUND) createNcbigeneCompoundFromXml(content, drop = drop) else NULL)
	})
}
