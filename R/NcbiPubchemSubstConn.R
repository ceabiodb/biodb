# vi: fdm=marker ts=4 et cc=80 tw=80

# NcbiPubchemSubstConn {{{1
################################################################################

#' NCBI PubChem Substance connector class.
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
