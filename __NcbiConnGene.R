library(RCurl)
source('NcbiGeneEntry.R')

##################
# GET GENE ENTRY #
##################

NcbiConn$methods(
	getGeneEntry = function(id) {
		url <- paste('http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=gene&id=', id, '&rettype=xml&retmode=text', sep='')
		xml <- getURL(url, useragent = useragent)
		entry <- NcbiGeneEntry$new(xmlstr = xml, conn = .self)

		# Check if an error occured
		if (entry$hasError()) return(NULL)

		# Check ID, because if no gene is found with the specified ID, it returns the first gene encountered.
		return(if (entry$getId() == id) entry else NULL)
	}
)

