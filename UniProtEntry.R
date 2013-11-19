library(XML)

UniProtEntry <- setRefClass("UniProEntry", fields = list(xml = "XMLInternalDocument", ns = "character"))

###############
# CONSTRUCTOR #
###############

UniProtEntry$methods(
	initialize = function(xmlstr, ...) {
		# The XML is passed as string, and parsed now. 'asText' means that first argument is a string containing the XML.
		xml <<- xmlInternalTreeParse(xmlstr, asText = TRUE)
		ns <<- c(uniprot = "http://uniprot.org/uniprot") # The namespace of the XML (will be used by XPath)
		callSuper(...) # calls super-class initializer with remaining parameters
	}
)

#############
# ACCESSION #
#############

UniProtEntry$methods(
	getAccession = function() {
		return(xpathSApply(xml, "//uniprot:accession", xmlValue, namespaces = ns))
	}
)

########
# NAME #
########

UniProtEntry$methods(
	getName = function() {
		return(xpathSApply(xml, "/uniprot:uniprot/uniprot:entry/uniprot:name", xmlValue, namespaces = ns))
	}
)

##########
# LENGTH #
##########

UniProtEntry$methods(
	getLength = function() {
		return(xpathSApply(xml, "//uniprot:sequence", xmlGetAttr, 'length', namespaces = ns))
	}
)

########
# MASS #
########

UniProtEntry$methods(
	getMass = function() {
		return(xpathSApply(xml, "//uniprot:sequence", xmlGetAttr, 'mass', namespaces = ns))
	}
)
