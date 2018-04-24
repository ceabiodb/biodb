# vi: fdm=marker

#' @include JsonEntry.R

# Class declaration {{{1
################################################################

PeakforestMassEntry <- methods::setRefClass("PeakforestMassEntry", contains = "JsonEntry")

# Constructor {{{1
################################################################

PeakforestMassEntry$methods( initialize = function(...) {

	callSuper(...)

	.self$addParsingExpression('ACCESSION', "id")
	.self$addParsingExpression('MSMODE', "polarity")
	.self$addParsingExpression('MSDEV', c('analyzerMassSpectrometerDevice', 'instrumentName'))
	.self$addParsingExpression('MSDEVTYPE', c('analyzerMassSpectrometerDevice', 'ionAnalyzerType'))
	.self$addParsingExpression('MSTYPE', 'type')
	.self$addParsingExpression('msprecmz', 'parentIonMZ')
	.self$addParsingExpression('CHROM.COL.NAME', c('liquidChromatography', 'columnName'))
	.self$addParsingExpression('CHROM.COL.ID', c('liquidChromatography', 'columnCode'))
	.self$addParsingExpression('CHROM.COL.CONSTRUCTOR', c('liquidChromatography', 'columnConstructorAString'))
	.self$addParsingExpression('CHROM.COL.LENGTH', c('liquidChromatography', 'columnLength'))
	.self$addParsingExpression('CHROM.COL.DIAMETER', c('liquidChromatography', 'columnDiameter'))
	.self$addParsingExpression('CHROM.COL.RT.MIN', 'RTmin')
	.self$addParsingExpression('CHROM.COL.RT.MAX', 'RTmax')
	.self$addParsingExpression('CHROM.COL.METHOD.PROTOCOL', c('liquidChromatography', 'methodProtocol'))
})

# Parse fields after {{{1
################################################################

PeakforestMassEntry$methods( .parseFieldsAfter = function(parsed.content) {

	# Set peaks
	if ('peaks' %in% names(parsed.content) && length(parsed.content$peaks) > 0) {

		# Creaate empty peaks data frame
		peaks <- data.frame(mz = double(), ri = double(), deltaPPM = double(), theoricalMass = double(), composition = character(), attribution = character(), stringsAsFactors = FALSE)

		# Loop on all peaks
		for (p in parsed.content$peaks) {

			# Set empty values to NA
			peak <- list()
			for (field in colnames(peaks))
				if (field %in% names(p)) {
					if (length(p[[field]]) > 0)
						peak[[field]] <- p[[field]]
					else
						peak[[field]] <- as.vector(NA, mode = class(peaks[[field]]))
				}
			peak <- data.frame(peak, stringsAsFactors = FALSE)

			# Append peak to peaks data frame
			peaks <- rbind(peaks, peak)
		}

		# Set right column names
		colnames(peaks) <- c('peak.mz', 'peak.relative.intensity', 'peak.error.ppm', 'peak.mass', 'peak.comp', 'peak.attr')
		.self$setFieldValue('peaks', peaks)
		.self$setFieldValue('nb.peaks', nrow(peaks))
	}

	# Set retention time unit
	if (.self$hasField('chrom.rt.min') || .self$hasField('chrom.rt.max'))
		.self$setField('chrom.rt.unit', 'min')

	# Parse compound IDs
	if ('listOfCompounds' %in% names(parsed.content))
		for (c in parsed.content$listOfCompounds)
			.self$appendFieldValue('peakforest.compound.id', c$id)

	# Set MS level
	if ('fragmentationLevelString' %in% names(parsed.content)) {
		if (parsed.content$fragmentationLevelString == 'MS2')
			.self$setFieldValue('ms.level', 2)
		else
			.self$message('caution', paste('Unknown MS type "', parsed.content$fragmentationLevelString,'" for Peakforest entry "', .self$getFieldValue('accession'), '".', sep = ''))
	}
	else
		.self$setFieldValue('ms.level', 1)

	# Get precursor
	if ( ! .self$hasField('msprecmz') && .self$hasField('peaks')) {
		prec <- .self$getFieldValue('peaks')[['peak.attr']] %in% c('[M+H]+', '[M-H]-')
		if (any(prec))
			.self$setFieldValue('msprecmz', .self$getFieldValue('peaks')[prec, 'peak.mz'])
	}
})
