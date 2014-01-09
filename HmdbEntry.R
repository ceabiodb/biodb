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
