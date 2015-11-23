source('BiodbConn.R')
source('UniProtEntry.R')

#####################
# CLASS DECLARATION #
#####################

UniProtConn <- setRefClass("UniProtConn", contains = "BiodbConn")

###############################
# DOWNLOAD ENTRY FILE CONTENT #
###############################

# Download an entry description as a file content, from the public database.
# id        The ID of the entry for which to download file content.
# RETURN    The file content describing the entry.
UniProtConn$methods(
	.doDownloadEntryFileContent = function(id) {
		url <- paste0('http://www.uniprot.org/uniprot/', id, '.xml')
		xml <- .self$.getUrl(url)
		return(xml)
})

################
# CREATE ENTRY #
################

# Creates an Entry instance from file content.
# file_content  A file content, downloaded from the public database.
# RETURN        An Entry instance.
UniProtConn$methods(
	.doCreateEntry = function(file_content) {
		entry <- createUniProtEntryFromXml(file_content)
		return(entry)
})
