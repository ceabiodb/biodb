# vi: fdm=marker

#' @include BiodbEntry.R

# Class declaration {{{1
################################################################

BiodbJsonEntry <- methods::setRefClass("BiodbJsonEntry", contains = 'BiodbEntry')

# Constructor {{{1
################################################################

BiodbJsonEntry$methods( initialize = function(...) {

	callSuper(...)
	.self$.abstract.class('BiodbJsonEntry')
})

# Do parse content {{{1
################################################################

BiodbJsonEntry$methods( .doParseContent = function(content) {

	# Parse JSON
	json <- jsonlite::fromJSON(content, simplifyDataFrame = FALSE)	

	return(json)
})

# Parse fields step 1 {{{1
################################################################

BiodbJsonEntry$methods( .parseFieldsStep1 = function(parsed.content) {

	# Get parsing expressions
	parsing.expr <- .self$getParent()$getPropertyValue('parsing.expr')

	# Set fields
	for (field in names(parsing.expr)) {

		x <- parsed.content

		# Go along path
		found.value <- TRUE
		for (t in parsing.expr[[field]])
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
