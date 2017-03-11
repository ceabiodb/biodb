# vi: fdm=marker

#' @include BiodbEntry.R

# Class declaration {{{1
################################################################

TxtEntry <- methods::setRefClass("TxtEntry", contains = 'BiodbEntry')

# Constructor {{{1
################################################################

TxtEntry$methods( initialize = function(...) {

	callSuper(...)
})

# Parse content {{{1
################################################################

TxtEntry$methods( parseContent = function(content) {

	# Get lines of content
	lines <- strsplit(content, "\n")[[1]]

	# Loop on lines
	for (s in lines) {

		# Test generic regex
		for (field in names(.self$.parsing.expr)) {
			g <- stringr::str_match(s, .self$.parsing.expr[[field]])
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

	.self$.afterParseContent()
})

# After parse content {{{1
################################################################

TxtEntry$methods( .afterParseContent = function(xml) {
})
