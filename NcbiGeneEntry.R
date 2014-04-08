library(XML)
source('BioDbEntry.R')

#####################
# CLASS DECLARATION #
#####################

NcbiGeneEntry <- setRefClass("NcbiGeneEntry", contains = "BioDbEntry", fields = list(symbol = "character", location = "character", fullname = "character", synonyms = "character", ccds_id = "character"))

###############
# CONSTRUCTOR #
###############

NcbiGeneEntry$methods(
	initialize = function(ccds_id = NA_character_, symbol = NA_character_, location = NA_character_, fullname = NA_character_, synonyms = NA_character_, ...) {
		ccds_id <<- if ( ! is.null(ccds_id)) ccds_id else NA_character_
		symbol <<- if ( ! is.null(symbol)) symbol else NA_character_
		location <<- if ( ! is.null(location)) location else NA_character_
		fullname <<- if ( ! is.null(fullname)) fullname else NA_character_
		synonyms <<- if ( ! is.null(synonyms)) synonyms else NA_character_
		callSuper(...)
})

###########
# CCDS ID #
###########

NcbiGeneEntry$methods(
	getCcdsId = function() {
		return(.self$ccds_id)
	}
)

##########
# SYMBOL #
##########

NcbiGeneEntry$methods(
	getSymbol = function() {
		return(.self$symbol)
	}
)

############
# LOCATION #
############

NcbiGeneEntry$methods(
	getLocation = function() {
		return(.self$location)
	}
)

######################
# OFFICIAL FULL NAME #
######################

NcbiGeneEntry$methods(
	getOfficialFullName = function() {
		return(.self$fullname)
	}
)

############
# SYNONYMS #
############

NcbiGeneEntry$methods(
	getSynonyms = function() {
		return(.self$synonyms)
	}
)

###########
# FACTORY #
###########

createNcbiGeneEntryFromXml <- function(xmlstr) {

	if (is.null(xmlstr) || is.na(xmlstr) || nchar(xmlstr) == 0)
		return(NULL)

	# Parse XML
	xml <-  xmlInternalTreeParse(xmlstr, asText = TRUE)

	# An error occured
	if (length(getNodeSet(xml, "//Error")) != 0 || length(getNodeSet(xml, "//ERROR")) != 0)
		return(NULL)

	# Get data
	id          <- unlist(xpathSApply(xml, "//Gene-track_geneid", xmlValue))
	kegg_id     <- unlist(xpathSApply(xml, "//Dbtag_db[text()='KEGG']/..//Object-id_str", xmlValue))
	synonyms    <- unlist(xpathSApply(xml, "//Gene-ref_syn_E", xmlValue))
	fullname    <- unlist(xpathSApply(xml, "//Gene-ref_desc", xmlValue))
	symbol      <- unlist(xpathSApply(xml, "//Gene-ref_locus", xmlValue))
	location    <- unlist(xpathSApply(xml, "//Gene-ref_maploc", xmlValue))
	ccds_id     <- .find_ccds_id(xml)

	return(if (is.null(id) || is.na(id)) NULL else NcbiGeneEntry$new(id = id, kegg_id = kegg_id, synonyms = synonyms, fullname = fullname, symbol = symbol, location = location, ccds_id = ccds_id))
}

#############
# FIND CCDS #
#############

.find_ccds_id <- function(xml) {

	# 1) Get all CCDS tags.
	ccds_elements <- getNodeSet(xml, "//Dbtag_db[text()='CCDS']/..//Object-id_str")

	# 2) If all CCDS are the same, go to point 4.
	ccds <- NA_character_
	for (e in ccds_elements) {
		current_ccds <- xmlValue(e)
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
			versions <- xpathSApply(e, "ancestor::Gene-commentary/Gene-commentary_label", xmlValue)
			if (length(versions) < 1) next
			current_version <- versions[[length(versions)]]
			if (is.na(version) || current_version < version) {
				version <- current_version
				ccds <- xmlValue(e)
			}
		}
	}

	return(ccds)
}
