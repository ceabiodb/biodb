# vi: fdm=marker

#' @include TxtEntry.R

# Class declaration {{{1
################################################################

ExpasyEnzymeEntry <- methods::setRefClass("ExpasyEnzymeEntry", contains = 'TxtEntry')

# Constructor {{{1
################################################################

ExpasyEnzymeEntry$methods( initialize = function(...) {

	superClass(...)

	regex[[BIODB.ACCESSION]] <- "^ID\\s+([0-9.]+)$"
	regex[[BIODB.NAME]]      <- "^DE\\s+(.+?)\\.?$"
	regex[[BIODB.SYNONYMS]]  <- "^AN\\s+(.+?)\\.?$" # Alternate names
	regex[[BIODB.CATALYTIC.ACTIVITY]]  <- "^CA\\s+(.+?)\\.?$"
	regex[[BIODB.COFACTOR]]  <- "^CF\\s+(.+?)\\.?$"
})

# Parse content {{{1
################################################################

ExpasyEnzymeEntry$methods( parseContent = function(content) {

	# XXX Content in EMBL Format ?

	# Define fields regex
	regex <- character()

	lines <- strsplit(content, "\n")[[1]]
	for (s in lines) {

		# Test generic regex
		for (field in names(regex)) {
			g <- stringr::str_match(s, regex[[field]])
			if ( ! is.na(g[1,1])) {
				if (.self$hasField(field)) {
					if (.self$getFieldCardinality(field) == BIODB.CARD.MANY)
						.self$setFieldValue(field, c(.self$getFieldValue(field), g[1,2]))
					else
						biodb$message(MSG.ERROR, paste("Cannot set multiple values into field \"", field, "\".", sep = ''))
				}
				else
					.self$setFieldValue(field, g[1,2])
				break
			}
		}
	}

	# Cofactors may be listed on a single line, separated by a semicolon.
	if (.self$hasField(BIODB.COFACTOR))
		.self$setFieldValue(BIODB.COFACTOR, unlist(strsplit(.self$getFieldValue(BIODB.COFACTOR), ' *; *')))
})
