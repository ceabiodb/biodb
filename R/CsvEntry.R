# vi: fdm=marker

#' @include BiodbEntry.R

# Class declaration {{{1
################################################################

CsvEntry <- methods::setRefClass("CsvEntry", contains = 'BiodbEntry')

# Constructor {{{1
################################################################

CsvEntry$methods( initialize = function(...) {

	callSuper(...)
})


# Do parse content {{{1
################################################################

CsvEntry$methods( .doParseContent = function(content) {

	# Split text in lines
	lines <- strsplit(content, "\n")[[1]]

	# Keys on first line
	keys <- strsplit(lines[[1]], ',')[[1]]

	# Values on second line
	values <- strsplit(lines[[2]], ',')[[1]]
	names(values) <- keys[seq(values)]

	return(values)
})

# Parse fields from expressions {{{1
################################################################

CsvEntry$methods( .parseFieldsFromExpr = function(parsed.content) {

	# Loop on all expressions
	for (field in names(.self$.parsing.expr))
		if (parsed.content[[.self$.parsing.expr[[field]]]] != '-')
			.self$setField(field, parsed.content[[.self$.parsing.expr[[field]]]])
})
