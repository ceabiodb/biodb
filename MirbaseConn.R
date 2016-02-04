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

# Download a compound description as a file content, from the public database.
# id        The ID of the compound for which to download file content.
# RETURN    The file content describing the compound.
MirbaseConn$methods(
	.doDownloadCompoundFileContent = function(id) {

		# Get accession number
		acc <- .self$.getAccessionNumberFromId(id)

# TODO Swap ACC and ID. Accession number if the Mirbase ID and ID is the name.
		# Get page
		xml <- NA_character_
		if ( ! is.null(acc) && ! is.na(acc))
			xml <- .self$.getUrl('http://www.mirbase.org/cgi-bin/mature.pl', params = c(mature_acc = acc))

		return(xml)
})

################
# CREATE COMPOUND #
################

# Creates a Compound instance from file content.
# file_content  A file content, downloaded from the public database.
# RETURN        A compound instance.
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

# TODO Keep this method for searching from name, but returns a list of compounds.
		# Get HTML
		htmlstr <- .self$.getUrl('http://www.mirbase.org/cgi-bin/query.pl', params = c(terms = id, submit = 'Search'))

		# Parse HTML
		xml <-  htmlTreeParse(htmlstr, asText = TRUE, useInternalNodes = TRUE)

		# Get accession number
		acc <- unlist(xpathSApply(xml, "//a[starts-with(.,'MIMAT')]", xmlValue))

		return(acc)
})
