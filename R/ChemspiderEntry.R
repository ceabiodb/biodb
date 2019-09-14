# vi: fdm=marker ts=4 et cc=80 tw=80

# ChemspiderEntry {{{1
################################################################################

# Declaration {{{2
################################################################################

#' ChemSpider entry class
#'
#' This is the entry class for the ChemSpider database.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Create a connector to ChemSpider
#' conn <- mybiodb$getFactory()$createConn('chemspider')
#'
#' # Get an entry
#' e <- conn$getEntry('2')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include BiodbJsonEntry.R
#' @export ChemspiderEntry
#' @exportClass ChemspiderEntry
ChemspiderEntry <- methods::setRefClass("ChemspiderEntry",
    contains="BiodbJsonEntry",

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(...) {

    callSuper(...)
}

))
