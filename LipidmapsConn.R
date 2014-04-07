source('BioDbConn.R')
source('LipidmapsEntry.R')

#####################
# CLASS DECLARATION #
#####################

LipidmapsConn <- setRefClass("LipidmapsConn", contains = "BioDbConn")

###############
# CONSTRUCTOR #
###############

LipidmapsConn$methods( initialize = function(...) {
	# From http://www.lipidmaps.org/data/structure/programmaticaccess.html:
	# If you write a script to automate calls to LMSD, please be kind and do not hit our server more often than once per 20 seconds. We may have to kill scripts that hit our server more frequently.
	callSuper(scheduler = UrlRequestScheduler$new(t = 20), ...)
})

###############################
# DOWNLOAD ENTRY FILE CONTENT #
###############################

# Download an entry description as a file content, from the public database.
# id        The ID of the entry for which to download file content.
# RETURN    The file content describing the entry.
LipidmapsConn$methods(
	.doDownloadEntryFileContent = function(id) {
		url <- paste0('http://www.lipidmaps.org/data/LMSDRecord.php?Mode=File&LMID=', id, '&OutputType=CSV&OutputQuote=No')
		csv <- .self$.getUrl(url)
		return(csv)
})

################
# CREATE ENTRY #
################

# Creates an Entry instance from file content.
# file_content  A file content, downloaded from the public database.
# RETURN        An Entry instance.
LipidmapsConn$methods(
	createEntry = function(file_content) {
		entry <- createLipidmapsEntryFromCsv(file_content)
		return(entry)
})
