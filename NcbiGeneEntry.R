library(XML)
source('XmlEntry.R', chdir = TRUE)
source('__NcbiConnDecl.R')
source('__NcbiConnCcds.R')

#####################
# CLASS DECLARATION #
#####################

NcbiGeneEntry <- setRefClass("NcbiGeneEntry", contains = "XmlEntry", fields = list(conn = "NcbiConn"))

######
# ID #
######

NcbiGeneEntry$methods(
	getId = function() {
		return(as.numeric(.self$getXmlTagContent("//Gene-track_geneid")))
	}
)

#############
# HAS ERROR #
#############

NcbiGeneEntry$methods(
	hasError = function() {
		return(length(getNodeSet(xml, "//Error")) != 0)
	}
)

##########
# SYMBOL #
##########

NcbiGeneEntry$methods(
	getSymbol = function() {
		return(.self$getXmlTagContent("//Gene-ref_locus"))
	}
)

############
# LOCATION #
############

NcbiGeneEntry$methods(
	getLocation = function() {
		return(.self$getXmlTagContent("//Gene-ref_maploc"))
	}
)

######################
# OFFICIAL FULL NAME #
######################

NcbiGeneEntry$methods(
	getOfficialFullName = function() {
		return(.self$getXmlTagContent("//Gene-ref_desc"))
	}
)

############
# SYNONYMS #
############

NcbiGeneEntry$methods(
	getSynonyms = function() {
		return(.self$getXmlTagContent("//Gene-ref_syn_E"))
	}
)

############
# SEQUENCE #
############

NcbiGeneEntry$methods(
	getSequence = function() {

		# 1) Get all CCDS tags.
		ccds_elements <- .self$getXmlNodes("//Dbtag_db[text()='CCDS']/..//Object-id_str")

		# 2) If all CCDS are the same, go to point 4.
		ccds <- NULL
		for (e in ccds_elements) {
			current_ccds <- xmlValue(e)
			if (is.null(ccds))
				ccds <- current_ccds
			else {
				if (current_ccds != ccds) {
					ccds <- NULL
					break
				}
			}
		}

		# 3) There are several CCDS values, we need to find the best one (i.e.: the most current one).
		if (is.null(ccds)) {
			# For each CCDS, look for the parent Gene-commentary tag. Then look for the text content of the Gene-commentary_label which is situed under. Ignore CCDS that have no Gene-commentary_label associated. Choose the CCDS that has the smallest Gene-commentary_label in alphabetical order.
			version <- NULL
			for (e in ccds_elements) {
				versions <- xpathSApply(e, "ancestor::Gene-commentary/Gene-commentary_label", xmlValue)
				if (length(versions) < 1) next
				current_version <- versions[[length(versions)]]
				if (is.null(version) || current_version < version) {
					version <- current_version
					ccds <- xmlValue(e)
				}
			}
		}

		# 4) get the sequence associated with this CCDS
		if (is.null(ccds)) return(NULL)
		return(.self$conn$getCcdsEntry(ccds)$getNucleotideSequence())
	}
)

