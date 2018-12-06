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

	# Read all CSV file, including header line, into a data frame. The header line will then be the first line. This is to avoid first column to be intrepretated as row names by read.table in case the header line contains one less field than the second line.
	df <- read.table(text = content, header = FALSE, row.names = NULL, sep = .self$.sep, quote = '', stringsAsFactors = FALSE, na.strings = .self$.na.strings, fill = TRUE, check.names = FALSE, comment.char = '')

	# Now name the columns
	if (nrow(df) >= 1) {

		# Remove unnamed columns
		df <- df[, ! is.na(df[1, ])]

		# Set colnames
		colnames(df) <- df[1, ]
		df <- df[seq(nrow(df)) != 1, ]
	}

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

	# Get parsing expressions
	parsing.expr <- .self$getParent()$.getParsingExpressions()

	# Loop on all expressions
	for (field in names(parsing.expr)) {

		# Is field in columns?
		if (parsing.expr[[field]] %in% names(parsed.content)) {

			# Get value
			v <- parsed.content[[parsing.expr[[field]]]]

			# Is value considered NA?
 			if ( ! is.null(.self$.na.strings) && length(.self$.na.strings >= 1) && ! all(is.na(.self$.na.strings)))
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
