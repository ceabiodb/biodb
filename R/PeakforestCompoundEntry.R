# vi: fdm=marker

#' @include JsonEntry.R

# Class declaration {{{1
################################################################

PeakforestCompoundEntry <- methods::setRefClass("PeakforestCompoundEntry", contains = "JsonEntry")

# Constructor {{{1
################################################################

PeakforestCompoundEntry$methods( initialize = function(...) {
	callSuper(...)
})

# Parse fields step 2 {{{1
################################################################

PeakforestCompoundEntry$methods( .parseFieldsStep2 = function(parsed.content) {

	# HMDB null
	if (.self$hasField('hmdb.metabolites.id')) {
		v <- .self$getFieldValue('hmdb.metabolites.id')
		v <- v[v != 'HMDBnull']
		if (length(v) > 0)
			.self$setFieldValue('hmdb.metabolites.id', v)
		else
			.self$removeField('hmdb.metabolites.id')
	}

	# ChEBI IDs
	if (.self$hasField('chebi.id')) {
		v <- .self$getFieldValue('chebi.id')
		v <- sub('^CHEBI:', '', v)
		.self$setFieldValue('chebi.id', v)
	}
})
