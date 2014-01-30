source('BioDbConn.R')
source('HmdbEntry.R')

#####################
# CLASS DECLARATION #
#####################

HmdbConn <- setRefClass("HmdbConn", contains = "BioDbConn")

########################
# GET METABOLITE ENTRY #
########################

HmdbConn$methods(
	getEntry = function(id) {
		url <- paste('http://www.hmdb.ca/metabolites/', id, '.xml', sep='')
		xml <- .self$getUrl(url)
		entry <- HmdbEntry$new(xmlstr = xml)
		# Check if an error occured
		if (entry$hasError()) return(NULL)
		return(entry)
	}
)
