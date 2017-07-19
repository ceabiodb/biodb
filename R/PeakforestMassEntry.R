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
		colnames(peaks) <- c('PEAK.MZ', 'PEAK.RELATIVE.INTENSITY', 'PEAK.ERROR.PPM', 'PEAK.MASS', 'PEAK.COMP', 'PEAK.ATTR')
		.self$setFieldValue('PEAKS', peaks)
		.self$setFieldValue('NB.PEAKS', nrow(peaks))
	}

	# Parse compound IDs
	if ('listOfCompounds' %in% names(parsed.content))
		for (c in parsed.content$listOfCompounds)
			.self$appendFieldValue('peakforest.compound.id', c$id)
})
