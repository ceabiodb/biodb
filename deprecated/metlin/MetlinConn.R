source('BioDbConn.R')
source('MetlinEntry.R')

#####################
# CLASS DECLARATION #
#####################

MetlinConn <- setRefClass("MetlinConn", contains = "BioDbConn")

###############
# CONSTRUCTOR #
###############

MetlinConn$methods( initialize = function(...) {
	callSuper(scheduler = UrlRequestScheduler$new(t = 1), ...)
})

###############################
# DOWNLOAD ENTRY FILE CONTENT #
###############################

# Download an entry description as a file content, from the public database.
# id        The ID of the entry for which to download file content.
# RETURN    The file content describing the entry.
MetlinConn$methods(
	.doDownloadEntryFileContent = function(id) {
		html <- .self$.getUrl('http://metlin.scripps.edu/metabo_info.php', params = c(molid = as.character(id)))
	print('HTML =============================================================================')
	print(html)
		return(html)
})

################
# CREATE ENTRY #
################

# Creates an Entry instance from file content.
# file_content  A file content, downloaded from the public database.
# RETURN        An Entry instance.
MetlinConn$methods(
	createEntry = function(file_content) {
	print('CONTENT =============================================================================')
	print(file_content)
		entry <- createMetlinEntryFromHtml(file_content)
		return(entry)
})
