if ( ! exists('LipdmapsConn')) { # Do not load again if already loaded

	source('BiodbConn.R')
	source('LipidmapsCompound.R')

	#####################
	# CLASS DECLARATION #
	#####################

	LipidmapsConn <- setRefClass("LipidmapsConn", contains = "BiodbConn")

	###############
	# CONSTRUCTOR #
	###############

	LipidmapsConn$methods( initialize = function(...) {
		# From http://www.lipidmaps.org/data/structure/programmaticaccess.html:
		# If you write a script to automate calls to LMSD, please be kind and do not hit our server more often than once per 20 seconds. We may have to kill scripts that hit our server more frequently.
		callSuper(scheduler = UrlRequestScheduler$new(t = 20), ...)
	})

	##########################
	# GET ENTRY CONTENT TYPE #
	##########################

	LipidmapsConn$methods( getEntryContentType = function(type) {
		return(RBIODB.CSV)
	})

	#####################
	# GET ENTRY CONTENT #
	#####################

	LipidmapsConn$methods( getEntryContent = function(type, id) {

		if (type == RBIODB.COMPOUND) {

			# Initialize return values
			content <- rep(NA_character_, length(id))

			# Request
			content <- vapply(id, function(x) .self$.scheduler$getUrl(get.lipidmaps.compound.url(x)), FUN.VALUE = '')

			return(content)
		}

		return(NULL)
	})

	################
	# CREATE ENTRY #
	################

	LipidmapsConn$methods( createEntry = function(type, content, drop = TRUE) {
		return(if (type == RBIODB.COMPOUND) createLipidmapsCompoundFromCsv(content, drop = drop) else NULL)
	})

	##############################
	# GET LIPIDMAPS COMPOUND URL #
	##############################

	get.lipidmaps.compound.url <- function(id) {

		url <- paste0('http://www.lipidmaps.org/data/LMSDRecord.php?Mode=File&LMID=', id, '&OutputType=CSV&OutputQuote=No')
	
		return(url)
	}
}
