if ( ! exists('MassbankConn')) { # Do not load again if already loaded

	source('RemotedbConn.R')
	source('MassdbConn.R')
	source('MassbankSpectrum.R')

	#####################
	# CLASS DECLARATION #
	#####################
	
	MassbankConn <- setRefClass("MassbankConn", contains = c("RemotedbConn", "MassdbConn"))

	##########################
	# GET ENTRY CONTENT TYPE #
	##########################

	MassbankConn$methods( getEntryContentType = function(type) {
		return(if (type == BIODB.SPECTRUM) BIODB.TXT else NULL) 
	})

	#####################
	# GET ENTRY CONTENT #
	#####################
	
	MassbankConn$methods( getEntryContent = function(type, ids) {

		if (type == BIODB.SPECTRUM) {

			URL.MAX.LENGTH <- 2083

			# Initialize return values
			content <- rep(NA_character_, length(ids))

			# Loop on all
			n <- 0
			while (n < length(ids)) {

				# Request
				accessions <- ids[(n + 1):length(ids)]
				x <- get.entry.url(class = BIODB.MASSBANK, accession = accessions, content.type = BIODB.TXT, max.length = URL.MAX.LENGTH)
				xmlstr <- .self$.scheduler$getUrl(x$url)
				n <- n + x$n

				# Parse XML and get text
				if ( ! is.na(xmlstr)) {
					library(XML)
					xml <-  xmlInternalTreeParse(xmlstr, asText = TRUE)
					ns <- c(ax21 = "http://api.massbank/xsd")
					returned.ids <- xpathSApply(xml, "//ax21:id", xmlValue, namespaces = ns)
					content[match(returned.ids, ids)] <- xpathSApply(xml, "//ax21:info", xmlValue, namespaces = ns)
				}
			}

			return(content)
		}

		return(NULL)
	})
	
	################
	# CREATE ENTRY #
	################
	
	# Creates a Spectrum instance from file content.
	# content       A file content, downloaded from the public database.
	# RETURN        A spectrum instance.
	MassbankConn$methods( createEntry = function(type, content, drop = TRUE) {
		return(if (type == BIODB.SPECTRUM) createMassbankSpectrumFromTxt(content, drop = drop) else NULL)
	})

	#################
	# GET MZ VALUES #
	#################
	
	MassbankConn$methods( getMzValues = function(mode = NULL, max.results = NA_integer_) {
	})
}
