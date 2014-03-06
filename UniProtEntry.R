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
	getAccessionIds = function() {
		return(.self$getXmlTagContent("//uniprot:accession"))
	}
)

######
# ID #
######

UniProtEntry$methods(
	getId = function() {
		ids <- .self$getAccessionIds()
		return(if (length(ids) > 1) ids[1] else ids)
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

############
# FULLNAME #
############

UniProtEntry$methods(
	getFullName = function() {
		names <- .self$getXmlTagContent("//uniprot:protein//uniprot:fullName")
		return(if (length(names) > 1) names[1] else names)
	}
)

###############
# GENE SYMBOL #
###############

UniProtEntry$methods(
	getGeneSymbol = function() {
		names <- .self$getXmlTagContent("//uniprot:gene/uniprot:name")
		return(if (length(names) > 1) names[1] else names)
	}
)

##########
# LENGTH #
##########

UniProtEntry$methods(
	getLength = function() {
		return(.self$getXmlTagAttribute("//uniprot:entry/uniprot:sequence", 'length'))
	}
)

########
# MASS #
########

UniProtEntry$methods(
	getMass = function() {
		return(.self$getXmlTagAttribute("//uniprot:entry/uniprot:sequence", 'mass'))
	}
)

###########
# KEGG ID #
###########

# <dbReference type="KEGG" id="hsa:12"/>
UniProtEntry$methods(
	getKeggId = function() {
		id <- .self$getXmlTagAttribute("//uniprot:dbReference[@type='KEGG']", 'id')
		if (length(id) == 0) id <- NA_character_
		return(id)
	}
)

#############
# ENZIME ID #
#############

# <dbReference type="EC" id="2.3.1.43"/>
UniProtEntry$methods(
	getEnzymeId = function() {
		id <- .self$getXmlTagAttribute("//uniprot:dbReference[@type='EC']", 'id')
		if (length(id) == 0) id <- NA_character_
		return(id)
	}
)
