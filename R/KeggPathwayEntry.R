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

	# Class
	.self$.parseMultilinesField(field = 'pathway.class', tag = 'CLASS', parsed.content = parsed.content, strip.chars = ' ', split.char = ';')

	# Module IDs
	.self$.parseModuleIds(parsed.content)

	# Compound IDs
	.self$.parseCompoundIds(parsed.content)
})
