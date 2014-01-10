library(RCurl)
source('EnzymeEntry.R')

#####################
# CLASS DECLARATION #
#####################

EnzymeConn <- setRefClass("EnzymeConn",
						fields = list(useragent = "character"))

#############
# GET ENTRY #
#############

EnzymeConn$methods(
	getEntry = function(id) {
		url <- paste('http://enzyme.expasy.org/EC/', id, '.txt', sep='')
		txt <- getURL(url, useragent = useragent)
		entry <- createEnzymeEntryFromText(txt)
		return(entry)
	}
)
