source('BioDbConn.R')
source('LipidmapsEntry.R')

#####################
# CLASS DECLARATION #
#####################

LipidmapsConn <- setRefClass("LipidmapsConn", contains = "BioDbConn")

###############
# CONSTRUCTOR #
###############

LipidmapsConn$methods( initialize = function(...) {
	# From http://www.lipidmaps.org/data/structure/programmaticaccess.html:
	# If you write a script to automate calls to LMSD, please be kind and do not hit our server more often than once per 20 seconds. We may have to kill scripts that hit our server more frequently.
	callSuper(scheduler = UrlRequestScheduler$new(t = 20), ...)
})

#############
# GET ENTRY #
#############

LipidmapsConn$methods(
	getEntry = function(id) {
		url <- paste('http://www.lipidmaps.org/data/LMSDRecord.php?Mode=File&LMID=', id, '&OutputType=CSV&OutputQuote=Yes', sep='')
		csv <- .self$getUrl(url)
		entry <- createLipidmapsEntryFromCsv(csv)
		return(entry)
	}
)
