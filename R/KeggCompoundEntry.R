# vi: fdm=marker ts=4 et cc=80 tw=80

#' @include KeggEntry.R

# Class declaration {{{1
################################################################################

KeggCompoundEntry <- methods::setRefClass("KeggCompoundEntry", contains='KeggEntry')

# Initialize {{{1
################################################################################

KeggCompoundEntry$methods( initialize=function(...) {

    callSuper(...)
})

# Parse fields step 2 {{{1
################################################################################

KeggCompoundEntry$methods( .parseFieldsStep2=function(parsed.content) {

    # Name
    .self$.parseMultilinesField(field='name', tag='NAME', parsed.content=parsed.content, strip.chars=' ;', split.char=NA_character_)

    # Other KEGG IDs
    .self$.parseMultilinesField(field='kegg.reaction.id', tag='REACTION', parsed.content=parsed.content)
    .self$.parseMultilinesField(field='kegg.enzyme.id',   tag='ENZYME', parsed.content=parsed.content)
    .self$.parsePathwayIds(parsed.content=parsed.content)
})
