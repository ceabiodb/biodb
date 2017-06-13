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

	# Set precursor
	if (BIODB.PEAK.ATTR %in% colnames(peaks)) {
		precursors.attr <- .self$getParent()$.precursors
		precursors <- peaks[[BIODB.PEAK.ATTR]] %in% precursors.attr
		if (sum(precursors) == 1)
			.self$setFieldValue(BIODB.MSPRECMZ, peaks[precursors, BIODB.PEAK.MZ])
		else if (sum(precursors) > 1) {
			.self$message(MSG.CAUTION, paste("Found more than one precursor inside entry ", .self$getFieldValue(BIODB.ACCESSION, compute = FALSE), ': ', paste(peaks[precursors, BIODB.PEAK.ATTR], collapse = ", "), ". Trying to take the one with highest intensity.", sep = ''))
			strongest.precursor.mz <- NULL
			for (int.col in c(BIODB.PEAK.INTENSITY, BIODB.PEAK.RELATIVE.INTENSITY))
				if (int.col %in% colnames(peaks))
					strongest.precursor.mz <- peaks[precursors, BIODB.PEAK.MZ][[which(order(peaks[precursors, int.col], decreasing = TRUE) == 1)]]
			if (is.null(strongest.precursor.mz))
				.self$message(MSG.CAUTION, 'No intensity information found for choosing the strongest precursor.')
			else {
				.self$message(MSG.INFO, paste('Found strongest precursor:', strongest.precursor.mz, '.'))
				.self$setFieldValue(BIODB.MSPRECMZ, strongest.precursor.mz)
			}
		}
	}
})
