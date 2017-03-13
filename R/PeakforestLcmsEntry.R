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
