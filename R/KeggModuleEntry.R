# vi: fdm=marker ts=4 et cc=80 


# KeggModuleEntry {{{1
################################################################################

#' @include KeggEntry.R
KeggModuleEntry <- methods::setRefClass(
    "KeggModuleEntry",
    contains = 'KeggEntry')

# Public methods {{{2
################################################################################

methods = list(

# Initialize {{{3
################################################################################

initialize = function(...) {
    callSuper(...)
},

# Private methods {{{2
################################################################################

# Makes reference to entry, recurse {{{3
################################################################################

.makesRefToEntryRecurse = function(db, oid) {

    makes_ref <- FALSE

    if (db == 'kegg.enzyme' && .self$hasField('kegg.reaction.id')) {

        # We need to check that the oid is listed
        # in at least one of the reactions
        krc <- .self$getBiodb()$getFactory()$getConn('kegg.reaction')
        reaction.ids <- .self$getFieldValue('kegg.reaction.id')
        makes_ref <- krc$makesRefToEntry(reaction.ids, db = db, oid = oid,
                                         any = TRUE, recurse = TRUE)
    }

    return(makes_ref)
},

# Parse fields step 2 {{{3
################################################################################

KeggModuleEntry$methods( .parseFieldsStep2 = function(parsed.content) {

    # Name
    .self$.parseMultilinesField(field = 'name', tag = 'NAME', parsed.content = parsed.content, strip.chars = ' ;', split.char = NA_character_)

    # Compounds
    .self$.parseCompoundIds(parsed.content)

    # Reactions
    .self$.parseReactionIds(parsed.content)

    # Pathway
    .self$.parsePathwayIds(parsed.content)
}

))
