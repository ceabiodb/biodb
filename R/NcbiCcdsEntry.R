# vi: fdm=marker

#' @include HtmlEntry.R

# Class declaration {{{1
################################################################

NcbiCcdsEntry <- methods::setRefClass("NcbiCcdsEntry", contains = "HtmlEntry")

# Constructor {{{1
################################################################

NcbiCcdsEntry$methods( initialize = function(...) {

	callSuper(...)
})

# Parse fields after {{{1
################################################################

NcbiCcdsEntry$methods( .parseFieldsAfter = function(parsed.content) {

	if (length(XML::getNodeSet(parsed.content, "//*[starts-with(.,'No results found for CCDS ID ')]")) == 0) {
		.self$setFieldValue(BIODB.ACCESSION, XML::xpathSApply(parsed.content, "//input[@id='DATA']", XML::xmlGetAttr, "value"))
		.self$setFieldValue(BIODB.SEQUENCE, XML::xpathSApply(parsed.content, "//b[starts-with(.,'Nucleotide Sequence')]/../tt", XML::xmlValue))
	}
})

###########
# FACTORY #
###########

createNcbiCcdsEntryFromHtml <- function(biodb, contents, drop = TRUE) {

	entries <- list()

	for (html in contents) {

		# Create instance
		entry <- NcbiCcdsEntry$new(biodb = biodb)
	
		# Parse HTML
		xml <-  XML::htmlTreeParse(html, asText = TRUE, useInternalNodes = TRUE)

		entries <- c(entries, entry)
	}

	# Replace elements with no accession id by NULL
	entries <- lapply(entries, function(x) if (is.na(x$getField(BIODB.ACCESSION))) NULL else x)

	# If the input was a single element, then output a single object
	if (drop && length(contents) == 1)
		entries <- entries[[1]]

	return(entries)
}
