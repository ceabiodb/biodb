source('BioDbConn.R')
source('EnzymeEntry.R')

#####################
# CLASS DECLARATION #
#####################

EnzymeConn <- setRefClass("EnzymeConn", contains = "BioDbConn")

#############
# GET ENTRY #
#############

EnzymeConn$methods(
	getEntry = function(id) {
		url <- paste('http://enzyme.expasy.org/EC/', id, '.txt', sep='')
		txt <- .self$getUrl(url)
		entry <- createEnzymeEntryFromText(txt)
		return(entry)
	}
)
