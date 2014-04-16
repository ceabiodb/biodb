source('BioDbConn.R')
source('HmdbEntry.R')

#####################
# CLASS DECLARATION #
#####################

HmdbConn <- setRefClass("HmdbConn", contains = "BioDbConn")

###############################
# DOWNLOAD ENTRY FILE CONTENT #
###############################

# Download an entry description as a file content, from the public database.
# id        The ID of the entry for which to download file content.
# RETURN    The file content describing the entry.
HmdbConn$methods(
	.doDownloadEntryFileContent = function(id) {
		url <- paste0('http://www.hmdb.ca/metabolites/', id, '.xml')
		xml <- .self$.getUrl(url)
		return(xml)
})

################
# CREATE ENTRY #
################

# Creates an Entry instance from file content.
# file_content  A file content, downloaded from the public database.
# RETURN        An Entry instance.
HmdbConn$methods(
	.doCreateEntry = function(file_content) {
		entry <- createHmdbEntryFromXml(file_content)
		return(entry)
})
