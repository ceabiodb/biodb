library(RCurl)
library(XML)

UniProtConn <- setRefClass("UniProtConn",
						   fields = list(useragent = "character"))

UniProtConn$methods(
	getEntry = function(id) {
		url <- paste('http://www.uniprot.org/uniprot/', id, '.xml', sep='')
		xmlstr <- getURL(url, useragent=useragent)
		xmldoc <- xmlTreeParse(xmlstr, asText = TRUE)
		print(xmldoc)
#		xmldoc <- parseXMLAndAdd(xmlstr)
#		xmldoc <- parseXMLAndAdd("<xml><zozo>Coucou</zozo></xml>")
	}
)
