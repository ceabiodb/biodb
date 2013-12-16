library(RCurl)
source('UniProtEntry.R')

UniProtConn <- setRefClass("UniProtConn",
						   fields = list(useragent = "character"))

UniProtConn$methods(
	getEntry = function(id) {
		url <- paste('http://www.uniprot.org/uniprot/', id, '.xml', sep='')
		xml <- getURL(url, useragent = useragent)

		# If the entity doesn't exist (i.e.: no <id>.xml page), then it returns an HTML page
		if (grepl("^<!DOCTYPE html PUBLIC", xml, perl=TRUE))
			return(NULL)

		return(UniProtEntry$new(xmlstr = xml))
	}
)
