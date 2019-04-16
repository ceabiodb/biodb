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
	compound.ids = .self$.getTagLines(tag = 'COMPOUND', parsed.content = parsed.content)
	if (length(compound.ids) > 0) {
		compound.ids = sub('^\\s*(C[0-9]+)\\s+.*$', '\\1', compound.ids)
		.self$setFieldValue('kegg.compound.id', compound.ids)
	}

	# Reactions
	reaction.ids = .self$.getTagLines(tag = 'REACTION', parsed.content = parsed.content)
	if (length(reaction.ids) > 0) {
		reaction.ids = stringr::str_match_all(reaction.ids, '(^|[ +,])(R[0-9]+)')
		reaction.ids = unlist(lapply(reaction.ids, function(x) x[,3]))
		.self$setFieldValue('kegg.reaction.id', reaction.ids)
	}

	# Pathway
	pathway.ids = .self$.getTagLines(tag = 'PATHWAY', parsed.content = parsed.content)
	if (length(pathway.ids) > 0) {
		pathway.ids = sub('^\\s*(map[0-9]+)\\s+.*$', '\\1', pathway.ids)
		.self$setFieldValue('kegg.pathway.id', pathway.ids)
	}
})
