# vi: fdm=marker ts=4 et cc=80 tw=80

# NcbiPubchemSubstConn {{{1
################################################################################

# Declaration {{{2
################################################################################

#' NCBI PubChem Substance connector class.
#'
#' This is the connector class for a NCBI PubChen Substance database.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('ncbi.pubchem.subst')
#'
#' # Get an entry
#' e <- conn$getEntry('2')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include NcbiPubchemConn.R
#' @export NcbiPubchemSubstConn
#' @exportClass NcbiPubchemSubstConn
NcbiPubchemSubstConn <- methods::setRefClass("NcbiPubchemSubstConn",
    contains="NcbiPubchemConn",

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(...) {
    callSuper(db.name='substance', id.xmltag='PC-ID_id',
              entry.xmltag='PC-Substance', id.urlfield='sid',
              entrez.name='pcsubstance', ...)
}

))
