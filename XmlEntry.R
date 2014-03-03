library(XML)
source('Entry.R')

###########################
# XmlEntry ABSTRACT CLASS #
###########################

XmlEntry <- setRefClass("XmlEntry", contains = "Entry", fields = list(xml = "XMLInternalDocument", ns = "character", use_ns = "logical"))

###############
# CONSTRUCTOR #
###############

XmlEntry$methods(
	initialize = function(xmlstr = "<xml/>", namespaces = c(none = ""), html = FALSE, ...) {
		# The XML is passed as string, and parsed now. 'asText' means that first argument is a string containing the XML.
		if (html)
			xml <<-  htmlTreeParse(xmlstr, asText = TRUE, useInternalNodes = TRUE)
#xml <<-  xmlInternalTreeParse(xmlstr, asText = TRUE, isHTML = TRUE, error = htmlErrorHandler())
		else
			xml <<-  xmlInternalTreeParse(xmlstr, asText = TRUE)
		ns <<- namespaces # The namespace of the XML (will be used by XPath)
		use_ns <<- sum(nchar(namespaces)) > 0
		callSuper(...) # calls super-class initializer with remaining parameters
	}
)

####################################
# SEARCH XML FOR A TAG'S ATTRIBUTE #
####################################

XmlEntry$methods( getXmlTagAttribute = function(path, attr) {
	if (use_ns)
		return(xpathSApply(xml, path, xmlGetAttr, attr, namespaces = ns))
	else
		return(xpathSApply(xml, path, xmlGetAttr, attr))
})

##################################
# SEARCH XML FOR A TAG'S CONTENT #
##################################

XmlEntry$methods( getXmlTagContent = function(path) {
	if (use_ns)
		return(xpathSApply(xml, path, xmlValue, namespaces = ns))
	else
		return(xpathSApply(xml, path, xmlValue))
})

#####################################
# SEARCH XML FOR A TAG NODE ELEMENT #
#####################################

XmlEntry$methods( getXmlNodes = function(path) {
	if (use_ns)
		return(getNodeSet(xml, path, namespaces = ns))
	else
		return(getNodeSet(xml, path))
})

########
# SAVE #
########

XmlEntry$methods(
	save = function(file) {
		saveXML(xml, file)
	}
)
