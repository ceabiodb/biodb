# vi: fdm=marker

#' @include XmlEntry.R

# Class declaration {{{1
################################################################

NcbiGeneEntry <- methods::setRefClass("NcbiGeneEntry", contains = "XmlEntry")

# Constructor {{{1
################################################################

NcbiGeneEntry$methods( initialize = function(...) {

	callSuper(...)

	.self$addParsingExpression(BIODB.ACCESSION, "//Gene-track_geneid")
	.self$addParsingExpression(BIODB.UNIPROT.ID, "//Gene-commentary_heading[text()='UniProtKB']/..//Dbtag_db[text()='UniProtKB/Swiss-Prot']/..//Object-id_str")
	.self$addParsingExpression(BIODB.LOCATION, "//Gene-ref_maploc")
	.self$addParsingExpression(BIODB.PROTEIN.DESCRIPTION, "//Gene-ref_desc")
	.self$addParsingExpression(BIODB.SYMBOL, "//Gene-ref_locus")
	.self$addParsingExpression(BIODB.SYNONYMS, "//Gene-ref_syn_E")
})

# Is parsed content correct {{{1
################################################################

NcbiGeneEntry$methods( .isParsedContentCorrect = function(parsed.content) {
	return(length(XML::getNodeSet(parsed.content, "//Error")) == 0 && length(XML::getNodeSet(parsed.content, "//ERROR")) == 0)
})

# Parse fields after {{{1
################################################################

NcbiGeneEntry$methods( .parseFieldsAfter = function(parsed.content) {

	# CCDS ID
	ccdsid <- .self$.find.ccds.id(parsed.content)
	if ( ! is.na(ccdsid))
		.self$setFieldValue(BIODB.NCBI.CCDS.ID, ccdsid)
})

# Find ccds id {{{1
################################################################

NcbiGeneEntry$methods( .find.ccds.id = function(parsed.content) {

	# 1) Get all CCDS tags.
	ccds_elements <- XML::getNodeSet(parsed.content, "//Dbtag_db[text()='CCDS']/..//Object-id_str")

	# 2) If all CCDS are the same, go to point 4.
	ccds <- NA_character_
	for (e in ccds_elements) {
		current_ccds <- XML::xmlValue(e)
		if (is.na(ccds))
			ccds <- current_ccds
		else {
			if (current_ccds != ccds) {
				ccds <- NA_character_
				break
			}
		}
	}

	# 3) There are several CCDS values, we need to find the best one (i.e.: the most current one).
	if (is.na(ccds)) {
		# For each CCDS, look for the parent Gene-commentary tag. Then look for the text content of the Gene-commentary_label which is situed under. Ignore CCDS that have no Gene-commentary_label associated. Choose the CCDS that has the smallest Gene-commentary_label in alphabetical order.
		version <- NA_character_
		for (e in ccds_elements) {
			versions <- XML::xpathSApply(e, "ancestor::Gene-commentary/Gene-commentary_label", XML::xmlValue)
			if (length(versions) < 1) next
			current_version <- versions[[length(versions)]]
			if (is.na(version) || current_version < version) {
				version <- current_version
				ccds <- XML::xmlValue(e)
			}
		}
	}

	return(ccds)
})
