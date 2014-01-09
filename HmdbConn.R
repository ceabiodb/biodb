library(RCurl)
source('HmdbEntry.R')

#####################
# CLASS DECLARATION #
#####################

HmdbConn <- setRefClass("HmdbConn",
						fields = list(useragent = "character"))

########################
# GET METABOLITE ENTRY #
########################

HmdbConn$methods(
	getEntry = function(id) {
		url <- paste('http://www.hmdb.ca/metabolites/', id, '.xml', sep='')
		xml <- getURL(url, useragent = useragent)
		entry <- HmdbEntry$new(xmlstr = xml)
		# Check if an error occured
		if (entry$hasError()) return(NULL)
		return(entry)
	}
)
