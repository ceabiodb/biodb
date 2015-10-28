if ( ! exists('KeggConn')) { # Do not load again if already loaded

	source('BioDbConn.R')
	source('KeggEntry.R')
	
	#####################
	# CLASS DECLARATION #
	#####################
	
	KeggConn <- setRefClass("KeggConn", contains = "BioDbConn")
	
	###############################
	# DOWNLOAD ENTRY FILE CONTENT #
	###############################
	
	# Download an entry description as a file content, from the public database.
	# id        The ID of the entry for which to download file content.
	# RETURN    The file content describing the entry.
	KeggConn$methods(
		.doDownloadEntryFileContent = function(id) {
			url <- get.kegg.entry.url(id, txt = TRUE)
			txt <- .self$.getUrl(url)
			return(txt)
	})
	
	################
	# CREATE ENTRY #
	################
	
	# Creates an Entry instance from file content.
	# file_content  A file content, downloaded from the public database.
	# RETURN        An Entry instance.
	KeggConn$methods( .doCreateEntry = function(file_content) {
		entry <- createKeggEntryFromText(file_content)
		return(entry)
	})
	
	######################
	# GET KEGG ENTRY URL #
	######################
	
	get.kegg.entry.url <- function(id, txt = FALSE) {

		if (txt)
			url <- paste0('http://rest.kegg.jp/get/', id)
		else
			url <- paste0('http://www.genome.jp/dbget-bin/www_bget?cpd:', id)
	
		return(url)
	}

} # end of load safe guard
