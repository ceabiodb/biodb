library(XML)
source('BiodbCompound.R')

#####################
# CLASS DECLARATION #
#####################

MirbaseCompound <- setRefClass("MirbaseCompound", contains = "BiodbCompound", fields = list(accession_number = "character", sequence = "character"))

###############
# CONSTRUCTOR #
###############

MirbaseCompound$methods(
	initialize = function(accession_number = NA_character_, sequence = NA_character_, ...) {
		accession_number <<- if ( ! is.null(accession_number)) accession_number else NA_character_
		sequence <<- if ( ! is.null(sequence)) sequence else NA_character_
		callSuper(...)
	}
)

####################
# ACCESSION NUMBER #
####################

MirbaseCompound$methods(
	getAccessionNumber = function() {
		return(.self$accession_number)
	}
)

############
# SEQUENCE #
############

MirbaseCompound$methods(
	getSequence = function() {
		return(.self$sequence)
	}
)

###########
# FACTORY #
###########

createMirbaseCompoundFromHtml <- function(htmlstr) {

	# Parse HTML
	xml <-  htmlTreeParse(htmlstr, asText = TRUE, useInternalNodes = TRUE)

	# Get data
	acc <- xpathSApply(xml, "//td[text()='Accession number']/../td[2]", xmlValue)
	id <- xpathSApply(xml, "//td[text()='ID']/../td[2]", xmlValue)
	seq <- xpathSApply(xml, "//td[text()='Sequence']/..//pre", xmlValue)

	return(if (is.na(id) || is.na(acc)) NULL else MirbaseCompound$new(id = id, accession_number = acc, sequence = seq))
}
