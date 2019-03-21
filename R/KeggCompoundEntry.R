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

# Parse multilines field {{{1
################################################################

KeggCompoundEntry$methods( .parseMultilinesField = function(field, tag, parsed.content, strip.chars = ' ', split.char = ' ') {

	value = character()

	# Loop on all lines of parsed content
	in.tag = FALSE
	for (line in parsed.content) {

		# Match
		regex = paste0(if (in.tag) "^" else paste0("^", tag), "\\s+(.*)\\s*$")
		g <- stringr::str_match(line, regex)

		# Exit loop
		if (is.na(g[1, 1]) && in.tag)
			break

		# Parse
		if ( ! is.na(g[1, 1])) {
			s = g[1, 2]
			if ( ! is.na(split.char))
				s = strsplit(s, split.char)[[1]]
			s = sub(paste0('[', strip.chars, ']+$'), '', sub(paste0('^[', strip.chars, ']+'), '', s))
			value = c(value, s)
			in.tag = TRUE
		}
	}

	# Set field value
	if (length(value) > 0)
		.self$setFieldValue(field, value)
})

# Parse fields step 2 {{{1
################################################################

KeggCompoundEntry$methods( .parseFieldsStep2 = function(parsed.content) {

	# Name
	.self$.parseMultilinesField(field = 'name', tag = 'NAME', parsed.content = parsed.content, strip.chars = ' ;', split.char = NA_character_)
})
