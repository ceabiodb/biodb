source('NcbiConn.R')
source('NcbiCcdsCompound.R')

#####################
# CLASS DECLARATION #
#####################

NcbiCcdsConn <- setRefClass("NcbiCcdsConn", contains = "NcbiConn")

###############################
# DOWNLOAD COMPOUND FILE CONTENT #
###############################

# Download an compound description as a file content, from the public database.
# id        The ID of the compound for which to download file content.
# RETURN    The file content describing the compound.
NcbiCcdsConn$methods(
	.doDownloadCompoundFileContent = function(id) {
		# There exists no CCDS connexion through the e-utilities, so we must connect the web server and get an HTML page.
		xml <- .self$.getUrl('https://www.ncbi.nlm.nih.gov/CCDS/CcdsBrowse.cgi', params = c(REQUEST = 'CCDS', GO = 'MainBrowse', DATA = id))
		return(xml)
})

################
# CREATE COMPOUND #
################

# Creates an Compound instance from file content.
# file_content  A file content, downloaded from the public database.
# RETURN        An Compound instance.
NcbiCcdsConn$methods(
	.doCreateCompound = function(file_content) {
		compound <- createNcbiCcdsCompoundFromHtml(file_content)
		return(compound)
})
