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

# Do parse content {{{1
################################################################

TxtEntry$methods( .doParseContent = function(content) {

	# Get lines of content
	lines <- strsplit(content, "\n")[[1]]

	return(lines)
})

# Parse fields from expressions {{{1
################################################################

TxtEntry$methods( .parseFieldsFromExpr = function(parsed.content) {

	.self$.assert.not.null(parsed.content)
	.self$.assert.not.na(parsed.content)
	.self$.assert.not.null(.self$.parsing.expr)
	.self$.assert.not.na(.self$.parsing.expr)
	.self$.assert.not.null(names(.self$.parsing.expr))

	# Loop on all parsing expressions
	for (field in names(.self$.parsing.expr)) {

		# Match whole content 
		g <- stringr::str_match(parsed.content, .self$.parsing.expr[[field]])

		# Get positive results
		results <- g[ ! is.na(g[,1]), , drop = FALSE]

		# Any match ?
		if (nrow(results) > 0)
			.self$setFieldValue(field, results[,2])
	}
})
