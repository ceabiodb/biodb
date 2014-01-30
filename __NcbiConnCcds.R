source('BioDbConn.R')
source('NcbiCcdsEntry.R')

##################
# GET CCDS ENTRY #
##################

NcbiConn$methods(
	getCcdsEntry = function(id) {
		# There is exists no CCDS connexion through the e-utilities, so we must connect the web server and get an HTML page.
		url <- paste('http://www.ncbi.nlm.nih.gov/CCDS/CcdsBrowse.cgi?REQUEST=CCDS&GO=MainBrowse&DATA=', id, sep='')
		xml <- .self$getUrl(url)
		entry <- NcbiCcdsEntry$new(xmlstr = xml)

		# Check if an error occured
		if (entry$hasError()) return(NULL)

		# Check ID
		return(if (entry$getId() == id) entry else NULL)
	}
)
