# vi: fdm=marker

#' @include BiodbEntry.R

# Class declaration {{{1
################################################################

TxtEntry <- methods::setRefClass("TxtEntry", contains = 'BiodbEntry')

# Constructor {{{1
################################################################

TxtEntry$methods( initialize = function(...) {

	callSuper(...)
	.self$.abstract.class('TxtEntry')
})

# Do parse content {{{1
################################################################

TxtEntry$methods( .doParseContent = function(content) {

	# Get lines of content
	lines <- strsplit(content, "\r?\n")[[1]]

	return(lines)
})

# Parse fields step 1 {{{1
################################################################

TxtEntry$methods( .parseFieldsStep1 = function(parsed.content) {

	# Get parsing expressions
	parsing.expr <- .self$getParent()$.getParsingExpressions()

	.self$.assert.not.null(parsed.content)
	.self$.assert.not.na(parsed.content)
	.self$.assert.not.null(parsing.expr)
	.self$.assert.not.na(parsing.expr)
	.self$.assert.not.null(names(parsing.expr))

	# Loop on all parsing expressions
	for (field in names(parsing.expr)) {

		# Match whole content 
		g <- stringr::str_match(parsed.content, parsing.expr[[field]])

		# Get positive results
		results <- g[ ! is.na(g[,1]), , drop = FALSE]

		# Any match ?
		if (nrow(results) > 0)
			.self$setFieldValue(field, results[,2])
	}
})
