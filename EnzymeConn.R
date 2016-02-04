source('BiodbConn.R')
source('EnzymeCompound.R')

#####################
# CLASS DECLARATION #
#####################

EnzymeConn <- setRefClass("EnzymeConn", contains = "BiodbConn")

###############################
# DOWNLOAD COMPOUND FILE CONTENT #
###############################

# Download an compound description as a file content, from the public database.
# id        The ID of the compound for which to download file content.
# RETURN    The file content describing the compound.
EnzymeConn$methods(
	.doDownloadCompoundFileContent = function(id) {
		url <- paste0('http://enzyme.expasy.org/EC/', id, '.txt')
		txt <- .self$.getUrl(url)
		return(txt)
})

################
# CREATE COMPOUND #
################

# Creates an Compound instance from file content.
# file_content  A file content, downloaded from the public database.
# RETURN        An Compound instance.
EnzymeConn$methods(
	.doCreateCompound = function(file_content) {
		compound <- createEnzymeCompoundFromText(file_content)
		return(compound)
})
