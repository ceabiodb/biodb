source('BioDbConn.R')
source('UniProtEntry.R')

#####################
# CLASS DECLARATION #
#####################

UniProtConn <- setRefClass("UniProtConn", contains = "BioDbConn")

#############
# GET ENTRY #
#############

UniProtConn$methods(
	getEntry = function(id) {
		url <- paste('http://www.uniprot.org/uniprot/', id, '.xml', sep='')
		xml <- .self$getUrl(url)

		# If the entity doesn't exist (i.e.: no <id>.xml page), then it returns an HTML page
		if (grepl("^<!DOCTYPE html PUBLIC", xml, perl=TRUE))
			return(NULL)

		return(UniProtEntry$new(xmlstr = xml))
	}
)
