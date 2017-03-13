# vi: fdm=marker

#' @include JsonEntry.R

# Class declaration {{{1
################################################################

PeakforestLcmsEntry <- methods::setRefClass("PeakforestLcmsEntry", contains = "JsonEntry")

# Constructor {{{1
################################################################

PeakforestLcmsEntry$methods( initialize = function(...) {

	callSuper(...)

	.self$addParsingExpression(BIODB.ACCESSION, "id")
	.self$addParsingExpression(BIODB.MSMODE, "polarity")
	.self$addParsingExpression(BIODB.MSDEV, c('analyzerMassSpectrometerDevice', 'instrumentName'))
	.self$addParsingExpression(BIODB.MSDEVTYPE, c('analyzerMassSpectrometerDevice', 'ionAnalyzerType'))
})

# Parse fields after {{{1
################################################################

PeakforestLcmsEntry$methods( .parseFieldsAfter = function(parsed.content) {

	# Set peaks
	if ('peaks' %in% names(parsed.content) && length(parsed.content$peaks) > 0) {
		peaks <- data.frame(mz = double(), rel.int = double(), error = double(), mass = double(), comp = character(), attr = character())
		colnames(peaks) <- c(BIODB.PEAK.MZ, BIODB.PEAK.RELATIVE.INTENSITY, BIODB.PEAK.ERROR.PPM, BIODB.PEAK.MASS, BIODB.PEAK.COMP, BIODB.PEAK.ATTR)
		for (p in parsed.content$peaks) {
			peak <- data.frame(mz = p$mz, rel.int = p$ri, error = p$deltaPPM, mass = p$theoricalMass, comp = p$composition, attr = p$attribution)
			colnames(peak) <- c(BIODB.PEAK.MZ, BIODB.PEAK.RELATIVE.INTENSITY, BIODB.PEAK.ERROR.PPM, BIODB.PEAK.MASS, BIODB.PEAK.COMP, BIODB.PEAK.ATTR)
			peaks <- rbind(peaks, peak)
		}
		.self$setFieldValue(BIODB.PEAKS, peaks)
		.self$setFieldValue(BIODB.NB.PEAKS, nrow(peaks))
	}
})
