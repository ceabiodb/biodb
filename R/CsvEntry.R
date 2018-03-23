# vi: fdm=marker

#' @include BiodbEntry.R

# Class declaration {{{1
################################################################

CsvEntry <- methods::setRefClass("CsvEntry", contains = 'BiodbEntry', fields = list( .sep = 'character', .na.strings = 'character'))

# Constructor {{{1
################################################################

CsvEntry$methods( initialize = function(sep = ',', na.strings = 'NA', ...) {

	callSuper(...)
	.self$.abstract.class('CsvEntry')

	.sep <<- sep
	.na.strings <<- na.strings
})


# Do parse content {{{1
################################################################

CsvEntry$methods( .doParseContent = function(content) {

	df <- read.table(text = content, header = TRUE, row.names = NULL, sep = .self$.sep, quote = '', stringsAsFactors = FALSE, na.strings = .self$.na.strings, fill = TRUE, check.names = FALSE, comment.char = '')

	return(df)
})

# Is parsed content correct {{{1
################################################################

CsvEntry$methods( .isParsedContentCorrect = function(parsed.content) {
	return(nrow(parsed.content) > 0)
})

# Parse fields from expressions {{{1
################################################################

CsvEntry$methods( .parseFieldsFromExpr = function(parsed.content) {

	# Loop on all expressions
	for (field in names(.self$.parsing.expr)) {

		# Is field in columns?
		if (.self$.parsing.expr[[field]] %in% names(parsed.content)) {

			# Get value
			v <- parsed.content[[.self$.parsing.expr[[field]]]]

			# Is value considered NA?
 			if ( ! is.na(.self$.na.strings))
				v[v %in% .self$.na.strings] <- NA

			# Remove NA values
			v <- v[ ! is.na(v)]

			# Remove duplicated values
			v <- v[ ! duplicated(v)]

			# Set value
			if (length(v) > 0 && any( ! is.na(v)))
				.self$setFieldValue(field, v)
		}
	}
})
