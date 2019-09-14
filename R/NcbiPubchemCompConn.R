# vi: fdm=marker ts=4 et cc=80 tw=80

# NcbiPubchemCompConn {{{1
################################################################################

# Declaration {{{2
################################################################################

#' NCBI PubChem Compound connector class.
#'
#' This is the connector class for a NCBI PubChen Compound database.
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
#' @include NcbiPubchemConn.R
#' @include BiodbCompounddbConn.R
#' @export NcbiPubchemCompConn
#' @exportClass NcbiPubchemCompConn
NcbiPubchemCompConn <- methods::setRefClass("NcbiPubchemCompConn",
    contains=c("NcbiPubchemConn", 'BiodbCompounddbConn'),

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(...) {
    callSuper(db.name='compound', id.xmltag='PC-CompoundType_id_cid',
              entry.xmltag='PC-Compound', id.urlfield='cid',
              entrez.name='pccompound', ...)
},

# Search compound {{{3
################################################################################

searchCompound=function(name=NULL, mass=NULL, mass.field=NULL, mass.tol=0.01,
                        mass.tol.unit='plain', max.results=NA_integer_) {
    # Overrides super class' method.

    .self$.checkMassField(mass=mass, mass.field=mass.field)

    term <- character()

    # Search by name
    if ( ! is.null(name))
        term <- paste0('"', name, '"', '[IUPACName]')

    # Search by mass
    if ( ! is.null(mass) && ! is.null(mass.field)) {

        mass.field <- .self$getBiodb()$getEntryFields()$getRealName(mass.field)

        if ( ! mass.field %in% c('monoisotopic.mass' ,'molecular.mass'))
            .self$caution('Mass field "', mass.field, '" is not handled.')

        else {
            if (mass.field == 'monoisotopic.mass')
                pubchem.mass.field <- 'MonoisotopicMass'
            else
                pubchem.mass.field <- 'MolecularWeight'

            if (mass.tol.unit == 'ppm') {
                mass.min <- mass * (1 - mass.tol * 1e-6)
                mass.max <- mass * (1 + mass.tol * 1e-6)
            } else {
                mass.min <- mass - mass.tol
                mass.max <- mass + mass.tol
            }

            mass.term <- paste0(mass.min, ':', mass.max, '[',
                                pubchem.mass.field, ']')

            if (length(term) > 0)
                term <- paste(term, 'AND', mass.term)
            else
                term <- mass.term
        }
    }

    # Set retmax
    if (is.na(max.results)) {
        xml <- .self$wsEsearch(term=term, retmax=0, retfmt='parsed')
        retmax <- as.integer(XML::xpathSApply(xml, "/eSearchResult/Count",
                                              XML::xmlValue))
        if (length(retmax) == 0)
            retmax <- NA_integer_
    }
    else
        retmax <- max.results

    # Send request
    ids <- .self$wsEsearch(term=term, retmax=retmax, retfmt='ids')

    return(ids)
}

))
