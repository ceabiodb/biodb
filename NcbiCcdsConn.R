source('NcbiConn.R')
source('NcbiCcdsEntry.R')

#####################
# CLASS DECLARATION #
#####################

NcbiCcdsConn <- setRefClass("NcbiCcdsConn", contains = "NcbiConn")

###############################
# DOWNLOAD ENTRY FILE CONTENT #
###############################

# Download an entry description as a file content, from the public database.
# id        The ID of the entry for which to download file content.
# RETURN    The file content describing the entry.
NcbiCcdsConn$methods(
	.doDownloadEntryFileContent = function(id) {
		# There exists no CCDS connexion through the e-utilities, so we must connect the web server and get an HTML page.
		url <- paste0('http://www.ncbi.nlm.nih.gov/CCDS/CcdsBrowse.cgi?REQUEST=CCDS&GO=MainBrowse&DATA=', id)
		xml <- .self$.getUrl(url)
		return(xml)
})

################
# CREATE ENTRY #
################

# Creates an Entry instance from file content.
# file_content  A file content, downloaded from the public database.
# RETURN        An Entry instance.
NcbiCcdsConn$methods(
	createEntry = function(file_content) {
		entry <- createNcbiCcdsEntryFromHtml(file_content)
		return(entry)
})
