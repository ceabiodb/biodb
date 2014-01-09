library(XML)
source('XmlEntry.R', chdir = TRUE)

#####################
# CLASS DECLARATION #
#####################

HmdbEntry <- setRefClass("HmdbEntry", contains = "XmlEntry")

#############
# HAS ERROR #
#############

HmdbEntry$methods(
	hasError = function() {
		return(length(getNodeSet(xml, "//error")) != 0)
	}
)

######
# ID #
######

HmdbEntry$methods(
	getId = function() {
		return(.self$getXmlTagContent("/metabolite/accession"))
	}
)

########
# NAME #
########

HmdbEntry$methods(
	getName = function() {
		return(.self$getXmlTagContent("/metabolite/name"))
	}
)

####################
# CHEMICAL FORMULA #
####################

HmdbEntry$methods(
	getFormula = function() {
		return(.self$getXmlTagContent("/metabolite/chemical_formula"))
	}
)

###############
# SUPER CLASS #
###############

HmdbEntry$methods(
	getSuperClass = function() {
		return(.self$getXmlTagContent("//super_class"))
	}
)
