source('NcbiConn.R')
source('NcbiGeneCompound.R')

#####################
# CLASS DECLARATION #
#####################

NcbiGeneConn <- setRefClass("NcbiGeneConn", contains = "NcbiConn")

###############################
# DOWNLOAD COMPOUND FILE CONTENT #
###############################

# Download an compound description as a file content, from the public database.
# id        The ID of the compound for which to download file content.
# RETURN    The file content describing the compound.
NcbiGeneConn$methods(
	.doDownloadCompoundFileContent = function(id) {

		if (as.numeric(id) <= 0)
			return(NA_character_)

		xml <- .self$.getUrl('http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi', params = c(db = 'gene', id = as.character(id), rettype = 'xml', retmode = 'text'))
		return(xml)
})

################
# CREATE COMPOUND #
################

# Creates an Compound instance from file content.
# file_content  A file content, downloaded from the public database.
# RETURN        An Compound instance.
NcbiGeneConn$methods(
	.doCreateCompound = function(file_content) {
		compound <- createNcbiGeneCompoundFromXml(file_content)
		return(compound)
})
