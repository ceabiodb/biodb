# vi: fdm=marker ts=4 et cc=80 

# KeggPathwayEntry {{{1
################################################################################

#' KEGG Pathway entry class.
#'
#' @include KeggEntry.R
#' @export KeggPathwayEntry
#' @exportClass KeggPathwayEntry
KeggPathwayEntry <- methods::setRefClass("KeggPathwayEntry",
    contains='KeggEntry',

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(...) {
    callSuper(...)
},

# Private methods {{{2
################################################################################

# Makes reference to entry, recurse {{{3
################################################################################

.makesRefToEntryRecurse=function(db, oid) {

    makes_ref <- FALSE

    if (db %in% c('kegg.compound', 'kegg.enzyme')
        && .self$hasField('kegg.module.id')) {

        # We need to check that oid is listed in at least one of the modules
        kmc <- .self$getBiodb()$getFactory()$getConn('kegg.module')
        module.ids <- .self$getFieldValue('kegg.module.id')
        makes_ref <- kmc$makesRefToEntry(module.ids, db=db, oid=oid,
                                         any=TRUE, recurse=TRUE)
    }

    return(makes_ref)
},

# Parse fields step 2 {{{3
################################################################

.parseFieldsStep2=function(parsed.content) {

    # Name
    .self$.parseMultilinesField(field='name',
                                tag='NAME',
                                parsed.content=parsed.content,
                                strip.chars=' ;',
                                split.char=NA_character_)

    # Class
    .self$.parseMultilinesField(field='pathway.class',
                                tag='CLASS',
                                parsed.content=parsed.content,
                                strip.chars=' ',
                                split.char=';')

    # Module IDs
    .self$.parseModuleIds(parsed.content)

    # Compound IDs
    .self$.parseCompoundIds(parsed.content)
}

))
