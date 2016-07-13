if ( ! exists('LipdmapsConn')) { # Do not load again if already loaded

	source('RemotedbConn.R')
	source('LipidmapsCompound.R')

	#####################
	# CLASS DECLARATION #
	#####################

	LipidmapsConn <- setRefClass("LipidmapsConn", contains = "RemotedbConn")

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
		return(BIODB.CSV)
	})

	#####################
	# GET ENTRY CONTENT #
	#####################

	LipidmapsConn$methods( getEntryContent = function(type, id) {

		if (type == BIODB.COMPOUND) {

			# Initialize return values
			content <- rep(NA_character_, length(id))

			# Request
			content <- vapply(id, function(x) .self$.scheduler$getUrl(get.entry.url(BIODB.LIPIDMAPS, x, content.type = BIODB.CSV)), FUN.VALUE = '')

			return(content)
		}

		return(NULL)
	})

	################
	# CREATE ENTRY #
	################

	LipidmapsConn$methods( createEntry = function(type, content, drop = TRUE) {
		return(if (type == BIODB.COMPOUND) createLipidmapsCompoundFromCsv(content, drop = drop) else NULL)
	})
}
