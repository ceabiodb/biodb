library(RCurl)
source('LipidmapsEntry.R')

#####################
# CLASS DECLARATION #
#####################

LipidmapsConn <- setRefClass("LipidmapsConn",
						fields = list(useragent = "character"))

#############
# GET ENTRY #
#############

LipidmapsConn$methods(
	getEntry = function(id) {
		url <- paste('http://www.lipidmaps.org/data/LMSDRecord.php?Mode=File&LMID=', id, '&OutputType=CSV&OutputQuote=Yes', sep='')
		csv <- getURL(url, useragent = useragent)
		entry <- createLipidmapsEntryFromCsv(csv)
		return(entry)
	}
)
