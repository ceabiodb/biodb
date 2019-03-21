# vi: fdm=marker

#' @include KeggEntry.R

# Class declaration {{{1
################################################################

KeggCompoundEntry <- methods::setRefClass("KeggCompoundEntry", contains = 'KeggEntry')

# Constructor {{{1
################################################################

KeggCompoundEntry$methods( initialize = function(...) {

	callSuper(...)
})

# Parse fields step 2 {{{1
################################################################

KeggCompoundEntry$methods( .parseFieldsStep2 = function(parsed.content) {

	# Name
	.self$.parseMultilinesField(field = 'name', tag = 'NAME', parsed.content = parsed.content, strip.chars = ' ;', split.char = NA_character_)

	# Other KEGG IDs
	.self$.parseMultilinesField(field = 'kegg.reaction.id', tag = 'REACTION', parsed.content = parsed.content)
	.self$.parseMultilinesField(field = 'kegg.enzyme.id',   tag = 'ENZYME', parsed.content = parsed.content)

	# Pathway
	pathway.ids = .self$.getTagLines(tag = 'PATHWAY', parsed.content = parsed.content)
	if (length(pathway.ids) > 0) {
		pathway.ids = sub('^\\s*(map[0-9]+)\\s+.*$', '\\1', pathway.ids)
		.self$setFieldValue('kegg.pathway.id', pathway.ids)
	}
})
