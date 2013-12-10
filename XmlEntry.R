library(XML)

###########################
# XmlEntry ABSTRACT CLASS #
###########################

XmlEntry <- setRefClass("XmlEntry", fields = list(xml = "XMLInternalDocument", ns = "character", use_ns = "logical"))

###############
# CONSTRUCTOR #
###############

XmlEntry$methods(
	initialize = function(xmlstr = "<xml/>", namespaces = c(none = ""), ...) {
		# The XML is passed as string, and parsed now. 'asText' means that first argument is a string containing the XML.
		xml <<-  xmlInternalTreeParse(xmlstr, asText = TRUE)
		ns <<- namespaces # The namespace of the XML (will be used by XPath)
		use_ns <<- sum(nchar(namespaces)) > 0
		callSuper(...) # calls super-class initializer with remaining parameters
	}
)

##############
# SEARCH XML #
##############

XmlEntry$methods( getXmlTagAttribute = function(path, attr) {
	if (use_ns)
		return(xpathSApply(xml, path, xmlGetAttr, attr, namespaces = ns))
	else
		return(xpathSApply(xml, path, xmlGetAttr, attr))
})

XmlEntry$methods( getXmlTagContent = function(path) {
	if (use_ns)
		return(xpathSApply(xml, path, xmlValue, namespaces = ns))
	else
		return(xpathSApply(xml, path, xmlValue))
})

########
# SAVE #
########

XmlEntry$methods(
	save = function(file) {
		saveXML(xml, file)
	}
)
