source('NcbiConn.R')
source('NcbiGeneEntry.R')

#####################
# CLASS DECLARATION #
#####################

NcbiGeneConn <- setRefClass("NcbiGeneConn", contains = "NcbiConn")

###############################
# DOWNLOAD ENTRY FILE CONTENT #
###############################

# Download an entry description as a file content, from the public database.
# id        The ID of the entry for which to download file content.
# RETURN    The file content describing the entry.
NcbiGeneConn$methods(
	.doDownloadEntryFileContent = function(id) {

		if (as.numeric(id) <= 0)
			return(NA_character_)

		xml <- .self$.getUrl('http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi', params = c(db = 'gene', id = as.character(id), rettype = 'xml', retmode = 'text'))
		return(xml)
})

################
# CREATE ENTRY #
################

# Creates an Entry instance from file content.
# file_content  A file content, downloaded from the public database.
# RETURN        An Entry instance.
NcbiGeneConn$methods(
	.doCreateEntry = function(file_content) {
		entry <- createNcbiGeneEntryFromXml(file_content)
		return(entry)
})
