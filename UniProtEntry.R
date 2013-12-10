library(XML)
source('XmlEntry.R', chdir = TRUE)

UniProtEntry <- setRefClass("UniProtEntry", contains = "XmlEntry")

###############
# CONSTRUCTOR #
###############

UniProtEntry$methods(
	initialize = function(...) {
		callSuper(namespaces = c(uniprot = "http://uniprot.org/uniprot"), ...)
	}
)

#############
# ACCESSION #
#############

UniProtEntry$methods(
	getAccession = function() {
		return(.self$getXmlTagContent("//uniprot:accession"))
	}
)

########
# NAME #
########

UniProtEntry$methods(
	getName = function() {
		return(.self$getXmlTagContent("/uniprot:uniprot/uniprot:entry/uniprot:name"))
	}
)

##########
# LENGTH #
##########

UniProtEntry$methods(
	getLength = function() {
		return(.self$getXmlTagAttribute("//uniprot:sequence", 'length'))
	}
)

########
# MASS #
########

UniProtEntry$methods(
	getMass = function() {
		return(.self$getXmlTagAttribute("//uniprot:sequence", 'mass'))
	}
)
