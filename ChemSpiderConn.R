if ( ! exists('ChemspiderConn')) { # Do not load again if already loaded

	source('RemotedbConn.R')
	source('ChemspiderCompound.R')
	
	#####################
	# CLASS DECLARATION #
	#####################
	
	ChemspiderConn <- setRefClass("ChemspiderConn", contains = "RemotedbConn")

	##########################
	# GET ENTRY CONTENT TYPE #
	##########################

	ChemspiderConn$methods( getEntryContentType = function(type) {
		return(BIODB.XML)
	})

	#####################
	# GET ENTRY CONTENT #
	#####################
	
	ChemspiderConn$methods( getEntryContent = function(type, ids) {

		# Debug
		.self$.print.debug.msg(paste0("Get entry content(s) for ", length(ids)," id(s)..."))

		if (type == BIODB.COMPOUND) {

			URL.MAX.LENGTH <- 2083

			# Loop on all
			n <- 0
			while (n < length(ids)) {

				# Get list of accession ids to retrieve
				accessions <- ids[(n + 1):length(ids)]

				# Create URL request
				x <- get.entry.url(class = BIODB.CHEMSPIDER, accession = accessions, content.type = BIODB.XML, max.length = URL.MAX.LENGTH, base.url = .self$.url, token = .self$.token)

				# Debug
				.self$.print.debug.msg(paste0("Send URL request for ", x$n," id(s)..."))

				# Send request
				print('******************** ChemspiderConn$getEntryContent')
				print(.self$.token)
				print(x$url)
				xmlstr <- .self$.scheduler$getUrl(x$url)
				print('************************************************************')
				print(xmlstr)

				# Increase number of entries retrieved
				n <- n + x$n

				# Parse XML and get included XML
				if ( ! is.na(xmlstr)) {
					library(XML)
					xml <-  xmlInternalTreeParse(xmlstr, asText = TRUE)
					returned.ids <- xpathSApply(xml, "//ExtendedCompoundInfo/CSID", xmlValue)
					content[match(returned.ids, ids)] <- xpathSApply(xml, "//ExtendedCompoundInfo")
				}

				# Debug
				.self$.print.debug.msg(paste0("Now ", length(ids) - n," id(s) left to be retrieved..."))
			}

			return(content)
		}

		return(NULL)
	})

	################
	# CREATE ENTRY #
	################
	
	ChemspiderConn$methods( createEntry = function(type, content, drop = TRUE) {
		return(if (type == BIODB.COMPOUND) createChemspiderCompoundFromXml(content, drop = drop) else NULL)
	})

	############################
	# GET CHEMSPIDER IMAGE URL #
	############################
	
	get.chemspider.image.url <- function(id) {
	
		url <- paste0('http://www.chemspider.com/ImagesHandler.ashx?w=300&h=300&id=', id)

		return(url)
	}
	
} # end of load safe guard

