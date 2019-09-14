# vi: fdm=marker ts=4 et cc=80 tw=80

# NcbiPubchemCompEntry {{{1
################################################################################

# Declaration {{{2
################################################################################

#' NCBI PubChem Compound entry class.
#'
#' This is the entry class for a NCBI PubChen Compound database.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('ncbi.pubchem.comp')
#'
#' # Get an entry
#' e <- conn$getEntry('2')
#'
#' # Terminate instance.
#' mybiodb$terminate()
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
