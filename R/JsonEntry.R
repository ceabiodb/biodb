# vi: fdm=marker

#' @include BiodbEntry.R

# Class declaration {{{1
################################################################

JsonEntry <- methods::setRefClass("JsonEntry", contains = 'BiodbEntry')

# Constructor {{{1
################################################################

JsonEntry$methods( initialize = function(...) {

	callSuper(...)
	.self$.abstract.class('JsonEntry')
})

# Do parse content {{{1
################################################################

JsonEntry$methods( .doParseContent = function(content) {


	# Parse JSON
	json <- jsonlite::fromJSON(content, simplifyDataFrame = FALSE)	

	return(json)
})

# Parse fields from expressions {{{1
################################################################

JsonEntry$methods( .parseFieldsFromExpr = function(parsed.content) {

	# Set fields
	for (field in names(.self$.parsing.expr)) {

		x <- parsed.content

		# Go along path
		found.value <- TRUE
		for (t in .self$.parsing.expr[[field]])
			if (t %in% names(x))
				x <- x[[t]]
			else {
				found.value <- FALSE
				break
			}

			# Set value
		if (found.value && length(x) == 1)
			.self$setFieldValue(field, x)
	}
})
