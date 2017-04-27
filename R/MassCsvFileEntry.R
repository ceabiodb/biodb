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

# Parse fields after {{{1
################################################################

MassCsvFileEntry$methods( .parseFieldsAfter = function(parsed.content) {

	# Make peak table
	peaks <- parsed.content[, colnames(parsed.content) %in% BIODB.PEAK.FIELDS]
	.self$setFieldValue(BIODB.PEAKS, peaks)
})
