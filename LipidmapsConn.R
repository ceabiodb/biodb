source('BiodbConn.R')
source('LipidmapsCompound.R')

#####################
# CLASS DECLARATION #
#####################

LipidmapsConn <- setRefClass("LipidmapsConn", contains = "BiodbConn")

###############
# CONSTRUCTOR #
###############

LipidmapsConn$methods( initialize = function(...) {
	# From http://www.lipidmaps.org/data/structure/programmaticaccess.html:
	# If you write a script to automate calls to LMSD, please be kind and do not hit our server more often than once per 20 seconds. We may have to kill scripts that hit our server more frequently.
	callSuper(scheduler = UrlRequestScheduler$new(t = 20), ...)
})

###############################
# DOWNLOAD COMPOUND FILE CONTENT #
###############################

# Download an compound description as a file content, from the public database.
# id        The ID of the compound for which to download file content.
# RETURN    The file content describing the compound.
LipidmapsConn$methods(
	.doDownloadCompoundFileContent = function(id) {
		csv <- .self$.getUrl('http://www.lipidmaps.org/data/LMSDRecord.php', params = c(Mode = 'File', LMID = id, OutputType = 'CSV', OutputQuote = 'No'))
		return(csv)
})

################
# CREATE COMPOUND #
################

# Creates an Compound instance from file content.
# file_content  A file content, downloaded from the public database.
# RETURN        An Compound instance.
LipidmapsConn$methods(
	.doCreateCompound = function(file_content) {
		compound <- createLipidmapsCompoundFromCsv(file_content)
		return(compound)
})
