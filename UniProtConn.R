library(RCurl)
source('UniProtEntry.R')

UniProtConn <- setRefClass("UniProtConn",
						   fields = list(useragent = "character"))

UniProtConn$methods(
	getEntry = function(id) {
		url <- paste('http://www.uniprot.org/uniprot/', id, '.xml', sep='')
		xml <- getURL(url, useragent = useragent)
		return(UniProtEntry$new(xmlstr = xml))
	}
)
