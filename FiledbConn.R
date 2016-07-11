if ( ! exists('FiledbConn')) {

	source('BiodbConn.R')
	
	#####################
	# CLASS DECLARATION #
	#####################
	
	FiledbConn <- setRefClass("FiledbConn", contains = "BiodbConn")

	##########################
	# GET ENTRY CONTENT TYPE #
	##########################

	FiledbConn$methods( getEntryContentType = function(type) {
		return(RBIODB.HTML)
	})
}
