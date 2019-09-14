# vi: fdm=marker ts=4 et cc=80 tw=80

# NcbiPubchemEntry {{{1
################################################################################

# Declaration {{{2
################################################################################

#' NCBI PubChem entry class.
#'
#' This the abstract entry class for all NCBI PubChem entry classes.
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
#' @include BiodbXmlEntry.R
#' @export NcbiPubchemEntry
#' @exportClass NcbiPubchemEntry
NcbiPubchemEntry <- methods::setRefClass("NcbiPubchemEntry",
    contains="BiodbXmlEntry",

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(...) {

    callSuper(...)
    .self$.abstractClass('NcbiPubchemEntry')
},

# Private methods {{{2
################################################################################

# Is parsed content correct {{{3
################################################################################

.isParsedContentCorrect=function(parsed.content) {
    fault <- XML::xpathSApply(parsed.content, "/Fault", XML::xmlValue)
    return(length(fault) == 0)
}

))
