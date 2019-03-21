# vi: fdm=marker

#' @include BiodbTxtEntry.R

# Class declaration {{{1
################################################################

KeggCompoundEntry <- methods::setRefClass("KeggCompoundEntry", contains = 'BiodbTxtEntry')

# Constructor {{{1
################################################################

KeggCompoundEntry$methods( initialize = function(...) {

	callSuper(...)
})

# Get tag lines {{{1
################################################################

KeggCompoundEntry$methods( .getTagLines = function(tag, parsed.content) {

	lines = character()

	# Loop on all lines of parsed content
	in.tag = FALSE
	for (line in parsed.content) {

		# Match
		regex = paste0(if (in.tag) "^" else paste0("^", tag), "\\s+(.*)\\s*$")
		g <- stringr::str_match(line, regex)

		# Exit loop
		if (is.na(g[1, 1]) && in.tag)
			break

		# Store line
		if ( ! is.na(g[1, 1])) {
			s = g[1, 2]
			lines = c(lines, s)
			in.tag = TRUE
		}
	}

	return(lines)
})

# Parse multilines field {{{1
################################################################

KeggCompoundEntry$methods( .parseMultilinesField = function(field, tag, parsed.content, strip.chars = ' ', split.char = ' ') {

	# Get tag lines
	lines = .self$.getTagLines(tag = tag, parsed.content = parsed.content)

	# Split on character
	if ( ! is.na(split.char))
		lines = unlist(strsplit(lines, paste0(split.char, "+"), perl = TRUE))

	value = sub(paste0('[', strip.chars, ']+$'), '', sub(paste0('^[', strip.chars, ']+'), '', lines))

	# Set field value
	if (length(value) > 0)
		.self$setFieldValue(field, value)
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
