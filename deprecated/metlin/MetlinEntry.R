library(XML)
source('BioDbEntry.R')

#####################
# CLASS DECLARATION #
#####################

MetlinEntry <- setRefClass("MetlinEntry", contains = 'BioDbEntry', fields = list(id = 'character'))

###########
# FACTORY #
###########

createMetlinEntryFromHtml <- function(htmlstr) {

	# Set XML namespace
	ns <- c(uniprot = "http://www.w3.org/1999/xhtml")

#print(htmlstr)
	# Parse HTML
	xml <- htmlTreeParse(htmlstr, asText = TRUE, useInternalNodes = TRUE)

	# An error occured
#	if (length(getNodeSet(xml, "//*[starts-with(.,'No results found for CCDS ID ')]")) != 0)
#		return(NULL)

	# Get data
	id <- xpathSApply(xml, "//b[text()='MID']", xmlValue, namespace = ns)
	print(id)
	id <- xpathSApply(xml, "//td[text()='MID']/../td", xmlValue)
	print('ID')
	print(id)

	return(if (is.na(id)) NULL else MetlinEntry$new(id = id))
}
