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
	if ( ! 'peak.mz' %in% colnames(peaks))
		for (mz.col in c('peak.mztheo', 'peak.mzexp'))
			if (mz.col %in% colnames(peaks))
				peaks[['peak.mz']] <- peaks[[mz.col]]

	# Chromatographic column id and name
	if (.self$hasField('chrom.col.name') && ! .self$hasField('chrom.col.id'))
		.self$setFieldValue('chrom.col.id', .self$getFieldValue('chrom.col.name'))
	if ( ! .self$hasField('chrom.col.name') && .self$hasField('chrom.col.id'))
		.self$setFieldValue('chrom.col.name', .self$getFieldValue('chrom.col.id'))

	# Set peaks table in field
	.self$setFieldValue('peaks', peaks)

	# Set precursor
	if ('peak.attr' %in% colnames(peaks)) {
		precursors.attr <- .self$getParent()$.precursors
		precursors <- peaks[['peak.attr']] %in% precursors.attr
		if (sum(precursors) == 1)
			.self$setFieldValue('msprecmz', peaks[precursors, 'peak.mz'])
		else if (sum(precursors) > 1) {
			.self$message('caution', paste("Found more than one precursor inside entry ", .self$getFieldValue('accession', compute = FALSE), ': ', paste(peaks[precursors, 'peak.attr'], collapse = ", "), ". Trying to take the one with highest intensity.", sep = ''))
			strongest.precursor.mz <- NULL
			for (int.col in c('peak.intensity', 'peak.relative.intensity'))
				if (int.col %in% colnames(peaks))
					strongest.precursor.mz <- peaks[precursors, 'peak.mz'][[which(order(peaks[precursors, int.col], decreasing = TRUE) == 1)]]
			if (is.null(strongest.precursor.mz))
				.self$message('caution', 'No intensity information found for choosing the strongest precursor.')
			else {
				.self$message('info', paste('Found strongest precursor:', strongest.precursor.mz, '.'))
				.self$setFieldValue('msprecmz', strongest.precursor.mz)
			}
		}
	}
})
