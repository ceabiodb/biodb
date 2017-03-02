#####################
# CLASS DECLARATION #
#####################

ExpasyEnzymeEntry <- methods::setRefClass("ExpasyEnzymeEntry", contains = 'BiodbEntry')

###########
# FACTORY #
###########

createExpasyEnzymeEntryFromTxt <- function(biodb, contents, drop = TRUE) {

	entries <- list()

	# XXX Content in EMBL Format ?

	# Define fields regex
	regex <- character()
	regex[[BIODB.ACCESSION]] <- "^ID\\s+([0-9.]+)$"
	regex[[BIODB.NAME]]      <- "^DE\\s+(.+?)\\.?$"
	regex[[BIODB.SYNONYMS]]  <- "^AN\\s+(.+?)\\.?$" # Alternate names
	regex[[BIODB.CATALYTIC.ACTIVITY]]  <- "^CA\\s+(.+?)\\.?$"
	regex[[BIODB.COFACTOR]]  <- "^CF\\s+(.+?)\\.?$"

	for (text in contents) {

		# Create instance
		entry <- ExpasyEnzymeEntry$new(biodb)

		lines <- strsplit(text, "\n")[[1]]
		for (s in lines) {

			# Test generic regex
			for (field in names(regex)) {
				g <- stringr::str_match(s, regex[[field]])
				if ( ! is.na(g[1,1])) {
					if (entry$hasField(field)) {
						if (entry$getFieldCardinality(field) == BIODB.CARD.MANY)
							entry$setFieldValue(field, c(entry$getFieldValue(field), g[1,2]))
						else
							stop(paste("Cannot set multiple values into field \"", field, "\".", sep = ''))
					}
					else
						entry$setFieldValue(field, g[1,2])
					break
				}
			}
		}

		# Cofactors may be listed on a single line, separated by a semicolon.
		if (entry$hasField(BIODB.COFACTOR))
			entry$setFieldValue(BIODB.COFACTOR, unlist(strsplit(entry$getFieldValue(BIODB.COFACTOR), ' *; *')))

		entries <- c(entries, entry)
	}

	# Replace elements with no accession id by NULL
	entries <- lapply(entries, function(x) if (is.na(x$getField(BIODB.ACCESSION))) NULL else x)

	# If the input was a single element, then output a single object
	if (drop && length(contents) == 1)
		entries <- entries[[1]]

	return(entries)
}
