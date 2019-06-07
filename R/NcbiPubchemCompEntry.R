# vi: fdm=marker ts=4 et cc=80

#' @include NcbiPubchemEntry.R

# Class declaration {{{1
################################################################################

NcbiPubchemCompEntry <- methods::setRefClass("NcbiPubchemCompEntry", contains="NcbiPubchemEntry")

# Initialize {{{1
################################################################################

NcbiPubchemCompEntry$methods( initialize=function(...) {
    callSuper(...)
})

# Parse fields step 2 {{{1
################################################################################

NcbiPubchemCompEntry$methods( .parseFieldsStep2=function(parsed.content) {

    # Set names
    names <- character()
    for (f in c('COMP.IUPAC.NAME.PREF', 'COMP.IUPAC.NAME.ALLOWED', 'COMP.IUPAC.NAME.TRAD', 'COMP.IUPAC.NAME.SYST', 'COMP.IUPAC.NAME.CAS'))
        if (.self$hasField(f))
            names <- c(names, .self$getFieldValue(f, compute=FALSE))
    if (length(names) > 0)
        .self$setFieldValue('name', names)
})
