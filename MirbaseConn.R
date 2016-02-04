library(XML)
source('BiodbConn.R')
source('MirbaseCompound.R')

#####################
# CLASS DECLARATION #
#####################

MirbaseConn <- setRefClass("MirbaseConn", contains = "BiodbConn")

###############################
# DOWNLOAD COMPOUND FILE CONTENT #
###############################

# Download an compound description as a file content, from the public database.
# id        The ID of the compound for which to download file content.
# RETURN    The file content describing the compound.
MirbaseConn$methods(
	.doDownloadCompoundFileContent = function(id) {

		# Get accession number
		acc <- .self$.getAccessionNumberFromId(id)

		# Get page
		xml <- NA_character_
		if ( ! is.null(acc) && ! is.na(acc))
			xml <- .self$.getUrl('http://www.mirbase.org/cgi-bin/mature.pl', params = c(mature_acc = acc))

		return(xml)
})

################
# CREATE COMPOUND #
################

# Creates an Compound instance from file content.
# file_content  A file content, downloaded from the public database.
# RETURN        An Compound instance.
MirbaseConn$methods(
	.doCreateCompound = function(file_content) {
		compound <- createMirbaseCompoundFromHtml(file_content)
		return(compound)
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
