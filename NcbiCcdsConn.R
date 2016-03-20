if ( ! exists('NcbiccdsConn')) { # Do not load again if already loaded

	source('BiodbConn.R')
	source('NcbiccdsCompound.R')

	#####################
	# CLASS DECLARATION #
	#####################

	NcbiccdsConn <- setRefClass("NcbiccdsConn", contains = "BiodbConn")

	###############
	# CONSTRUCTOR #
	###############

	NcbiccdsConn$methods( initialize = function(...) {
		# From NCBI E-Utility manual: "In order not to overload the E-utility servers, NCBI recommends that users post no more than three URL requests per second and limit large jobs to either weekends or between 9:00 PM and 5:00 AM Eastern time during weekdays".
		callSuper(scheduler = UrlRequestScheduler$new(n = 3), ...)
	})

	##########################
	# GET ENTRY CONTENT TYPE #
	##########################

	NcbiccdsConn$methods( getEntryContentType = function(type) {
		return(RBIODB.HTML)
	})

	#####################
	# GET ENTRY CONTENT #
	#####################
	
	NcbiccdsConn$methods( getEntryContent = function(type, id) {

		if (type == RBIODB.COMPOUND) {

			# Initialize return values
			content <- rep(NA_character_, length(id))

			# Request
			content <- vapply(id, function(x) .self$.scheduler$getUrl(get.entry.url(RBIODB.NCBICCDS, x)), FUN.VALUE = '')

			return(content)
		}

		return(NULL)
	})
	
	################
	# CREATE ENTRY #
	################
	
	NcbiccdsConn$methods( createEntry = function(type, content, drop = TRUE) {
		return(if (type == RBIODB.COMPOUND) createNcbiccdsCompoundFromHtml(content, drop = drop) else NULL)
	})
}
