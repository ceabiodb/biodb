# vi: fdm=marker

#' @include CsvEntry.R

# Class declaration {{{1
################################################################

MassCsvFileEntry <- methods::setRefClass("MassCsvFileEntry", contains = 'CsvEntry')

# Constructor {{{1
################################################################

MassCsvFileEntry$methods( initialize = function(...) {

	callSuper(sep = "\t", ...)

	for (field in names(.self$getParent()$.fields))
		if ( ! field %in% BIODB.PEAK.FIELDS)
			.self$addParsingExpression(field, .self$getParent()$.fields[[field]])
})

## Parse fields from expressions {{{1
#################################################################
#
#MassCsvFileEntry$methods( .parseFieldsFromExpr = function(parsed.content) {
#
#	# Translate custom fields to Biodb fields
#	colnames(df) <- vapply(colnames(df), function(x) if (x %in% .self$.fields) names(.self$.fields)[.self$.fields == x] else x, FUN.VALUE = '')
#
#	# Determine which columns contain constant value
#	entry.fields <- colnames(df)[vapply(colnames(df), function(x) sum(! duplicated(x)) == 1, FUN.VALUE = TRUE)]
#
#	# Remove peak columns from those columns (case where zero or only one peak in the table)
#	entry.fields <- entry.fields[ ! entry.fields %in% c(BIODB.PEAK.MZEXP, BIODB.PEAK.MZTHEO, BIODB.PEAK.COMP, BIODB.PEAK.ATTR)]
#
#	# Set entry fields
#	for (f in entry.fields)
#		entry$setFieldValue(f, df[1, f])
#})

# Parse fields after {{{1
################################################################

MassCsvFileEntry$methods( .parseFieldsAfter = function(parsed.content) {

	# Make peak table
	peaks <- parsed.content[, colnames(parsed.content) %in% BIODB.PEAK.FIELDS]
	.self$setFieldValue(BIODB.PEAKS, peaks)
})
