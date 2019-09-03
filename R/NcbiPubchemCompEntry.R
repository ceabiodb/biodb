# vi: fdm=marker ts=4 et cc=80 tw=80

# NcbiPubchemCompEntry {{{1
################################################################################

#' NCBI PubChem Compound entry class.
#'
#' @include NcbiPubchemEntry.R
#' @export NcbiPubchemCompEntry
#' @exportClass NcbiPubchemCompEntry
NcbiPubchemCompEntry <- methods::setRefClass("NcbiPubchemCompEntry",
    contains="NcbiPubchemEntry",

# Private methods {{{2
################################################################################

methods=list(

# Parse fields step 2 {{{3
################################################################################

.parseFieldsStep2=function(parsed.content) {

    # Set names
    names <- character()
    fields <- c('COMP.IUPAC.NAME.PREF', 'COMP.IUPAC.NAME.ALLOWED',
                'COMP.IUPAC.NAME.TRAD', 'COMP.IUPAC.NAME.SYST',
                'COMP.IUPAC.NAME.CAS')
    for (f in fields)
        if (.self$hasField(f))
            names <- c(names, .self$getFieldValue(f, compute=FALSE))
    if (length(names) > 0)
        .self$setFieldValue('name', names)
}

))
