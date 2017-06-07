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

	# Add MZ column if missing
	if ( ! BIODB.PEAK.MZ %in% colnames(peaks))
		for (mz.col in c(BIODB.PEAK.MZTHEO, BIODB.PEAK.MZEXP))
			if (mz.col %in% colnames(peaks))
				peaks[[BIODB.PEAK.MZ]] <- peaks[[mz.col]]

	# Set peaks table in field
	.self$setFieldValue(BIODB.PEAKS, peaks)
})
