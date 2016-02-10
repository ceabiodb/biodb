if ( ! exists('MassbankConn')) { # Do not load again if already loaded

	source('BiodbConn.R')
	source('MassbankSpectrum.R')

	#############
	# CONSTANTS #
	#############

	RBIODB.MASSBANK.WS.URL  <- "http://www.massbank.jp/api/services/MassBankAPI/getRecordInfo"

	#####################
	# CLASS DECLARATION #
	#####################
	
	MassbankConn <- setRefClass("MassbankConn", contains = "BiodbConn")

	##########################
	# GET ENTRY CONTENT TYPE #
	##########################

	MassbankConn$methods( getEntryContentType = function(type) {
		return(if (type == RBIODB.SPECTRUM) RBIODB.TXT else NULL) 
	})

	#####################
	# GET ENTRY CONTENT #
	#####################
	
	MassbankConn$methods( getEntryContent = function(type, id) {

		if (type == RBIODB.COMPOUND)
			return(NULL)

		# Initialize return values
		content <- rep(NA_character_, length(id))

		# Request
		xmlstr <- .self$.scheduler$getUrl(RBIODB.MASSBANK.WS.URL, params = c(ids = paste(id, collapse = ',')))

		# Parse XML and get text
		library(XML)
		xml <-  xmlInternalTreeParse(xmlstr, asText = TRUE)
		ns <- c(ax21 = "http://api.massbank/xsd")
		returned.ids <- xpathSApply(xml, "//ax21:id", xmlValue, namespaces = ns)
		content[match(returned.ids, id)] <- xpathSApply(xml, "//ax21:info", xmlValue, namespaces = ns)

		return(content)
	})
	
	################
	# CREATE ENTRY #
	################
	
	# Creates a Spectrum instance from file content.
	# content       A file content, downloaded from the public database.
	# RETURN        A spectrum instance.
	MassbankConn$methods( createEntry = function(type, content) {
		return(if (type == RBIODB.SPECTRUM) createMassbankSpectrumFromTxt(content) else NULL)
	})
}
