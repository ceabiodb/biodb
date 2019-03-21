# vi: fdm=marker

#' @include KeggEntry.R

# Class declaration {{{1
################################################################

KeggPathwayEntry <- methods::setRefClass("KeggPathwayEntry", contains = 'KeggEntry')

# Constructor {{{1
################################################################

KeggPathwayEntry$methods( initialize = function(...) {

	callSuper(...)
})

# Parse fields step 2 {{{1
################################################################

KeggPathwayEntry$methods( .parseFieldsStep2 = function(parsed.content) {

	# Name
	.self$.parseMultilinesField(field = 'name', tag = 'NAME', parsed.content = parsed.content, strip.chars = ' ;', split.char = NA_character_)

	#M odule 
	module.ids = .self$.getTagLines(tag = 'MODULE', parsed.content = parsed.content)
	if (length(module.ids) > 0) {
		module.ids = sub('^\\s*(M[0-9]+)\\s+.*$', '\\1', module.ids)
		.self$setFieldValue('kegg.module.id', module.ids)
	}
})
