source('BiodbConn.R')
source('UniProtCompound.R')

#####################
# CLASS DECLARATION #
#####################

UniProtConn <- setRefClass("UniProtConn", contains = "BiodbConn")

##################################
# DOWNLOAD COMPOUND FILE CONTENT #
##################################

# Download a compound description as a file content, from the public database.
# id        The ID of the compound for which to download file content.
# RETURN    The file content describing the compound.
UniProtConn$methods(
	.doDownloadCompoundFileContent = function(id) {
		url <- paste0('http://www.uniprot.org/uniprot/', id, '.xml')
		xml <- .self$.getUrl(url)
		return(xml)
})

###################
# CREATE COMPOUND #
###################

# Creates a Compound instance from file content.
# file_content  A file content, downloaded from the public database.
# RETURN        A compound instance.
UniProtConn$methods(
	.doCreateCompound = function(file_content) {
		compound <- createUniProtCompoundFromXml(file_content)
		return(compound)
})
