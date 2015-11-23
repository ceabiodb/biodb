library(XML)
source('BiodbEntry.R')

#####################
# CLASS DECLARATION #
#####################

MirbaseEntry <- setRefClass("MirbaseEntry", contains = "BiodbEntry", fields = list(accession_number = "character", sequence = "character"))

###############
# CONSTRUCTOR #
###############

MirbaseEntry$methods(
	initialize = function(accession_number = NA_character_, sequence = NA_character_, ...) {
		accession_number <<- if ( ! is.null(accession_number)) accession_number else NA_character_
		sequence <<- if ( ! is.null(sequence)) sequence else NA_character_
		callSuper(...)
	}
)

####################
# ACCESSION NUMBER #
####################

MirbaseEntry$methods(
	getAccessionNumber = function() {
		return(.self$accession_number)
	}
)

############
# SEQUENCE #
############

MirbaseEntry$methods(
	getSequence = function() {
		return(.self$sequence)
	}
)

###########
# FACTORY #
###########

createMirbaseEntryFromHtml <- function(htmlstr) {

	# Parse HTML
	xml <-  htmlTreeParse(htmlstr, asText = TRUE, useInternalNodes = TRUE)

	# Get data
	acc <- xpathSApply(xml, "//td[text()='Accession number']/../td[2]", xmlValue)
	id <- xpathSApply(xml, "//td[text()='ID']/../td[2]", xmlValue)
	seq <- xpathSApply(xml, "//td[text()='Sequence']/..//pre", xmlValue)

	return(if (is.na(id) || is.na(acc)) NULL else MirbaseEntry$new(id = id, accession_number = acc, sequence = seq))
}
