# vi: fdm=marker

#' @include KeggEntry.R

# Class declaration {{{1
################################################################

KeggReactionEntry <- methods::setRefClass("KeggReactionEntry", contains = 'KeggEntry')

# Constructor {{{1
################################################################

KeggReactionEntry$methods( initialize = function(...) {

	callSuper(...)
})

# Parse fields step 2 {{{1
################################################################

KeggReactionEntry$methods( .parseFieldsStep2 = function(parsed.content) {

	# Name
	.self$.parseMultilinesField(field = 'name', tag = 'NAME', parsed.content = parsed.content, strip.chars = ' ;', split.char = NA_character_)

	# Parse subtrates and products
	if (.self$hasField('equation')) {
		s = gsub(' ', '', .self$getFieldValue('equation')) # Remove spaces
		s = strsplit(strsplit(s, '<=>')[[1]], '\\+')
		if (length(s) == 2) {
			.self$setFieldValue('substrates', s[[1]])
			.self$setFieldValue('products', s[[2]])
		}
		else
			.self$message('caution', paste0('Unable to parse equation "', .self$getFieldValue('equation'), '" of KEGG reaction ', .self$getFieldValue('accession'), '.'))
	}
})
