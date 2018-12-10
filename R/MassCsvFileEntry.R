# vi: fdm=marker

#' @include CsvEntry.R

# Class declaration {{{1
################################################################

MassCsvFileEntry <- methods::setRefClass("MassCsvFileEntry", contains = 'CsvEntry')

# Constructor {{{1
################################################################

MassCsvFileEntry$methods( initialize = function(...) {

	callSuper(sep = "\t", ...)
})

# Parse fields after {{{1
################################################################

MassCsvFileEntry$methods( .parseFieldsAfter = function(parsed.content) {

	entry.fields <- .self$getBiodb()$getEntryFields()

	# Make peak table
	print('-------------------------------- MassCsvFileEntry::.parseFieldsAfter 01')
	print(parsed.content)
	peaks <- NULL
	for (field in entry.fields$getFieldNames()) {

		# Process only peak fields
		f <- entry.fields$get(field)
		if ( ! is.na(f$getGroup()) && f$getGroup() == 'peak') {

			# Is the field present in the parsed content data frame
			col.name <- if (field %in% names(.self$getParent()$.fields)) .self$getParent()$.fields[[field]] else field
			if (col.name %in% colnames(parsed.content)) {

				# Get vector of values
				values <- parsed.content[[col.name]]

				# Correct values
				values <- f$correctValue(values)

				# Add values to peak data frame
				if (is.null(peaks)) {
					peaks <- data.frame(x = values)
					colnames(peaks) <- field
				}
				else
					peaks[[field]] <- values
			}
		}
	}
	print('-------------------------------- MassCsvFileEntry::.parseFieldsAfter 10')
	print(peaks)

	# Add MZ column if missing
	if ( ! is.null(peaks) && ! 'peak.mz' %in% colnames(peaks))
		for (mz.col in c('peak.mztheo', 'peak.mzexp'))
			if (mz.col %in% colnames(peaks))
				peaks[['peak.mz']] <- peaks[[mz.col]]
	print('-------------------------------- MassCsvFileEntry::.parseFieldsAfter 11')
	print(peaks)

	# Set peaks table in field
	if ( ! is.null(peaks))
		.self$setFieldValue('peaks', peaks)

	# Chromatographic column id and name
	if (.self$hasField('chrom.col.name') && ! .self$hasField('chrom.col.id'))
		.self$setFieldValue('chrom.col.id', .self$getFieldValue('chrom.col.name'))
	if ( ! .self$hasField('chrom.col.name') && .self$hasField('chrom.col.id'))
		.self$setFieldValue('chrom.col.name', .self$getFieldValue('chrom.col.id'))

	# Set precursor
	if ('peak.attr' %in% colnames(peaks)) {
		precursors.attr <- .self$getParent()$getPrecursorFormulae()
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
