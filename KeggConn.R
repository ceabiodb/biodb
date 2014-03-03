source('BioDbConn.R')
source('KeggEntry.R')

#####################
# CLASS DECLARATION #
#####################

KeggConn <- setRefClass("KeggConn", contains = "BioDbConn")

#############
# GET ENTRY #
#############

KeggConn$methods(
	getEntry = function(id) {
		url <- paste('http://rest.kegg.jp/get/', id, sep='')
		txt <- .self$getUrl(url)
		entry <- createKeggEntryFromText(txt)
		return(entry)
	}
)
