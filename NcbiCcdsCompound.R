library(XML)
source('BiodbCompound.R')

#####################
# CLASS DECLARATION #
#####################

NcbiCcdsCompound <- setRefClass("NcbiCcdsCompound", contains = "BiodbCompound", fields = list(nucleotides = "character"))

###############
# CONSTRUCTOR #
###############

NcbiCcdsCompound$methods(
	initialize = function(nucleotides = NA_character_, ...) {
		nucleotides <<- if ( ! is.null(nucleotides)) nucleotides else NA_character_
		callSuper(...)
	}
)

#######################
# NUCLEOTIDE SEQUENCE #
#######################

NcbiCcdsCompound$methods(
	getNucleotideSequence = function() {
		return(.self$nucleotides)
	}
)

###########
# FACTORY #
###########

createNcbiCcdsCompoundFromHtml <- function(htmlstr) {

	# Parse HTML
	xml <-  htmlTreeParse(htmlstr, asText = TRUE, useInternalNodes = TRUE)

	# An error occured
	if (length(getNodeSet(xml, "//*[starts-with(.,'No results found for CCDS ID ')]")) != 0)
		return(NULL)

	# Get data
	id          <- xpathSApply(xml, "//input[@id='DATA']", xmlGetAttr, "value")
	nucleotides <- xpathSApply(xml, "//b[starts-with(.,'Nucleotide Sequence')]/../tt", xmlValue)

	return(if (is.na(id)) NULL else NcbiCcdsCompound$new(id = id, nucleotides = nucleotides))
}
