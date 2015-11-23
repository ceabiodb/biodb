library(XML)
source('BiodbConn.R')
source('MirbaseEntry.R')

#####################
# CLASS DECLARATION #
#####################

MirbaseConn <- setRefClass("MirbaseConn", contains = "BiodbConn")

###############################
# DOWNLOAD ENTRY FILE CONTENT #
###############################

# Download an entry description as a file content, from the public database.
# id        The ID of the entry for which to download file content.
# RETURN    The file content describing the entry.
MirbaseConn$methods(
	.doDownloadEntryFileContent = function(id) {

		# Get accession number
		acc <- .self$.getAccessionNumberFromId(id)

		# Get page
		xml <- NA_character_
		if ( ! is.null(acc) && ! is.na(acc))
			xml <- .self$.getUrl('http://www.mirbase.org/cgi-bin/mature.pl', params = c(mature_acc = acc))

		return(xml)
})

################
# CREATE ENTRY #
################

# Creates an Entry instance from file content.
# file_content  A file content, downloaded from the public database.
# RETURN        An Entry instance.
MirbaseConn$methods(
	.doCreateEntry = function(file_content) {
		entry <- createMirbaseEntryFromHtml(file_content)
		return(entry)
})

################################
# GET ACCESSION NUMBER FROM ID #
################################

MirbaseConn$methods(
	.getAccessionNumberFromId = function(id) {

		# Get HTML
		htmlstr <- .self$.getUrl('http://www.mirbase.org/cgi-bin/query.pl', params = c(terms = id, submit = 'Search'))

		# Parse HTML
		xml <-  htmlTreeParse(htmlstr, asText = TRUE, useInternalNodes = TRUE)

		# Get accession number
		acc <- unlist(xpathSApply(xml, "//a[starts-with(.,'MIMAT')]", xmlValue))

		return(acc)
})
