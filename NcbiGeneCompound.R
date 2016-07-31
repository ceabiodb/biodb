if ( ! exists('NcbigeneCompound')) { # Do not load again if already loaded

	source('BiodbEntry.R')
	source(file.path('..', 'r-lib', 'strhlp.R'), chdir = TRUE)

	#####################
	# CLASS DECLARATION #
	#####################

	NcbigeneCompound <- setRefClass("NcbigeneCompound", contains = "BiodbEntry")

	###########
	# FACTORY #
	###########

	createNcbigeneCompoundFromXml <- function(contents, drop = TRUE) {

		library(XML)

		compounds <- list()

		# Define xpath expressions
		xpath.expr <- character()
		xpath.expr[[BIODB.ACCESSION]] <- "//Gene-track_geneid"
		xpath.expr[[BIODB.KEGG.ID]] <- "/Dbtag_db[text()='KEGG']/..//Object-id_str"
		xpath.expr[[BIODB.UNIPROT.ID]] <- "//Gene-commentary_heading[text()='UniProtKB']/..//Dbtag_db[text()='UniProtKB/Swiss-Prot']/..//Object-id_str"
		xpath.expr[[BIODB.LOCATION]] <- "//Gene-ref_maploc"
		xpath.expr[[BIODB.PROTEIN.DESCRIPTION]] <- "//Gene-ref_desc"
		xpath.expr[[BIODB.SYMBOL]] <- "//Gene-ref_locus"
		xpath.expr[[BIODB.SYNONYMS]] <- "//Gene-ref_syn_E"

		for (content in contents) {

			# Create instance
			compound <- NcbigeneCompound$new()
		
			# Parse HTML
			xml <-  xmlInternalTreeParse(content, asText = TRUE)

			# An error occured
			if (length(getNodeSet(xml, "//Error")) == 0 && length(getNodeSet(xml, "//ERROR")) == 0) {

				# Test generic xpath expressions
				for (field in names(xpath.expr)) {
					v <- xpathSApply(xml, xpath.expr[[field]], xmlValue)
					if (length(v) > 0) {

						# Eliminate duplicates
						v <- v[ ! duplicated(v)]

						# Set field
						compound$setField(field, v)
					}
				}
			
				# CCDS ID
				ccdsid <- .find.ccds.id(xml)
				if ( ! is.na(ccdsid))
					compound$setField(BIODB.NCBI.CCDS.ID, ccdsid)
			}

			compounds <- c(compounds, compound)
		}

		# Replace elements with no accession id by NULL
		compounds <- lapply(compounds, function(x) if (is.na(x$getField(BIODB.ACCESSION))) NULL else x)

		# If the input was a single element, then output a single object
		if (drop && length(contents) == 1)
			compounds <- compounds[[1]]
	
		return(compounds)

		# Get data

	}

	################
	# FIND CCDS ID #
	################

	.find.ccds.id <- function(xml) {

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
}
