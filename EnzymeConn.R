source('BioDbConn.R')
source('EnzymeEntry.R')

#####################
# CLASS DECLARATION #
#####################

EnzymeConn <- setRefClass("EnzymeConn", contains = "BioDbConn")

###############################
# DOWNLOAD ENTRY FILE CONTENT #
###############################

# Download an entry description as a file content, from the public database.
# id        The ID of the entry for which to download file content.
# RETURN    The file content describing the entry.
EnzymeConn$methods(
	.doDownloadEntryFileContent = function(id) {
		url <- paste0('http://enzyme.expasy.org/EC/', id, '.txt')
		txt <- .self$.getUrl(url)
		return(txt)
})

################
# CREATE ENTRY #
################

# Creates an Entry instance from file content.
# file_content  A file content, downloaded from the public database.
# RETURN        An Entry instance.
EnzymeConn$methods(
	createEntry = function(file_content) {
		entry <- createEnzymeEntryFromText(file_content)
		return(entry)
})
