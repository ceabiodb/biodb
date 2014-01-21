library(XML)
source('XmlEntry.R', chdir = TRUE)

#####################
# CLASS DECLARATION #
#####################

NcbiCcdsEntry <- setRefClass("NcbiCcdsEntry", contains = "XmlEntry")

###############
# CONSTRUCTOR #
###############

NcbiCcdsEntry$methods(
	initialize = function(...) {
		callSuper(html = TRUE, ...)
	}
)

######
# ID #
######

NcbiCcdsEntry$methods(
	getId = function() {
		return(.self$getXmlTagAttribute("//input[@id='DATA']", "value"))
	}
)

#############
# HAS ERROR #
#############

NcbiCcdsEntry$methods(
	hasError = function() {
		return(length(getNodeSet(xml, "//*[starts-with(.,'No results found for CCDS ID ')]")) != 0)
	}
)

#######################
# NUCLEOTIDE SEQUENCE #
#######################

NcbiCcdsEntry$methods(
	getNucleotideSequence = function() {
		nucleotides <- .self$getXmlTagContent("//b[starts-with(.,'Nucleotide Sequence')]/../tt")
		return(nucleotides)
	}
)
