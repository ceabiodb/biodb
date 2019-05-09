# vi: fdm=marker

#' @include KeggEntry.R

# Class declaration {{{1
################################################################

KeggModuleEntry <- methods::setRefClass("KeggModuleEntry", contains = 'KeggEntry')

# Constructor {{{1
################################################################

KeggModuleEntry$methods( initialize = function(...) {

	callSuper(...)
})

# Parse fields step 2 {{{1
################################################################

KeggModuleEntry$methods( .parseFieldsStep2 = function(parsed.content) {

	# Name
	.self$.parseMultilinesField(field = 'name', tag = 'NAME', parsed.content = parsed.content, strip.chars = ' ;', split.char = NA_character_)

	# Compounds
	.self$.parseCompoundIds(parsed.content)

	# Reactions
	.self$.parseReactionIds(parsed.content)

	# Pathway
	.self$.parsePathwayIds(parsed.content)
})
