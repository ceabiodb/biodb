library(XML)
source('XmlEntry.R', chdir = TRUE)

#####################
# CLASS DECLARATION #
#####################

NcbiGeneEntry <- setRefClass("NcbiGeneEntry", contains = "XmlEntry")

######
# ID #
######

NcbiGeneEntry$methods(
	getId = function() {
		return(as.numeric(.self$getXmlTagContent("//Gene-track_geneid")))
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

##########
# SYMBOL #
##########

NcbiGeneEntry$methods(
	getSymbol = function() {
		return(.self$getXmlTagContent("//Gene-ref_locus"))
	}
)
