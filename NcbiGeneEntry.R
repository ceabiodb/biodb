library(XML)

NcbiGeneEntry <- setRefClass("NcbiGeneEntry", fields = list(xml = "XMLInternalDocument"))

###############
# CONSTRUCTOR #
###############

NcbiGeneEntry$methods(
	initialize = function(xmlstr, ...) {
		# The XML is passed as string, and parsed now. 'asText' means that first argument is a string containing the XML.
		xml <<- xmlInternalTreeParse(xmlstr, asText = TRUE)
		callSuper(...) # calls super-class initializer with remaining parameters
	}
)

######
# ID #
######

NcbiGeneEntry$methods(
	getId = function() {
		return(as.numeric(xpathSApply(xml, "//Gene-track_geneid", xmlValue)))
	}
)

#############
# HAS ERROR #
#############

NcbiGeneEntry$methods(
	hasError = function() {
		return(length(getNodeSet(xml, "//Error")) != 0)
	}
)

########
# SAVE #
########

NcbiGeneEntry$methods(
	save = function(file) {
		saveXML(xml, file)
	}
)

##########
# SYMBOL #
##########

NcbiGeneEntry$methods(
	getSymbol = function() {
		return(xpathSApply(xml, "//Gene-ref_locus", xmlValue))
	}
)
