# vi: fdm=marker ts=4 et cc=80 tw=80

# NcbiPubchemSubstEntry {{{1
################################################################################

# Declaration {{{2
################################################################################

#' NCBI PubChem Substance entry class.
#'
#' This is the entry class for a NCBI PubChen Substance database.
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
#' @include NcbiPubchemEntry.R
#' @export NcbiPubchemSubstEntry
#' @exportClass NcbiPubchemSubstEntry
NcbiPubchemSubstEntry <- methods::setRefClass("NcbiPubchemSubstEntry",
    contains="NcbiPubchemEntry")
