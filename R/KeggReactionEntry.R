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

})
