if ( ! exists('PubchemCompound')) { # Do not load again if already loaded

	source('BiodbEntry.R')

	#####################
	# CLASS DECLARATION #
	#####################

	PubchemCompound <- setRefClass("PubchemCompound", contains = "BiodbEntry")

	#####################
	# SUBSTANCE FACTORY #
	#####################

	createPubchemSubstanceFromXml <- function(contents, drop = TRUE) {

		library(XML)

		compounds <- list()
	}

	####################
	# COMPOUND FACTORY #
	####################

	createPubchemCompoundFromXml <- function(contents, drop = TRUE) {

		library(XML)

		compounds <- list()

		# Define xpath expressions
		xpath.expr <- character()
		xpath.expr[[BIODB.ACCESSION]] <- "//PC-CompoundType_id_cid"
		xpath.expr[[BIODB.INCHI]] <- "//PC-Urn_label[text()='InChI']/../..//PC-InfoData_value_sval"
		xpath.expr[[BIODB.INCHIKEY]] <- "//PC-Urn_label[text()='InChIKey']/../..//PC-InfoData_value_sval"
		xpath.expr[[BIODB.FORMULA]] <- "//PC-Urn_label[text()='Molecular Formula']/../..//PC-InfoData_value_sval"
		xpath.expr[[BIODB.MASS]] <- "//PC-Urn_label[text()='Mass']/../..//PC-InfoData_value_fval"
		xpath.expr[[BIODB.NAME]] <- "//PC-Urn_label[text()='IUPAC Name']/../PC-Urn_name[text()='Systematic']/../..//PC-InfoData_value_sval"

		for (content in contents) {

			# Create instance
			compound <- PubchemCompound$new()

			if ( ! is.null(content) && ! is.na(content)) {

				# Parse XML
				xml <-  xmlInternalTreeParse(content, asText = TRUE)

				# Unknown compound
				fault <- xpathSApply(xml, "/Fault", xmlValue, namespaces = ns)
				if (length(fault) == 0) {

					# Test generic xpath expressions
					for (field in names(xpath.expr)) {
						v <- xpathSApply(xml, xpath.expr[[field]], xmlValue, namespaces = ns)
						if (length(v) > 0)
							compound$setField(field, v)
					}
				}
			}

			compounds <- c(compounds, compound)
		}

		# Replace elements with no accession id by NULL
		compounds <- lapply(compounds, function(x) if (is.na(x$getField(BIODB.ACCESSION))) NULL else x)

		# If the input was a single element, then output a single object
		if (drop && length(contents) == 1)
			compounds <- compounds[[1]]
	
		return(compounds)
	}
}
