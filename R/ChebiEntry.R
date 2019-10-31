# vi: fdm=marker ts=4 et cc=80 tw=80

# ChebiEntry {{{1
################################################################################

# Declaration {{{2
################################################################################

#' ChEBI entry class.
#'
#' This is the entry class for ChEBI database.
#'
#' @seealso Super class \code{\link{BiodbXmlEntry}}.
#' 
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Create a connector to ChEBI
#' conn <- mybiodb$getFactory()$createConn('chebi')
#'
#' # Get an entry
#' e <- conn$getEntry('15440')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include BiodbXmlEntry.R
#' @export ChebiEntry
#' @exportClass ChebiEntry
ChebiEntry <- methods::setRefClass("ChebiEntry",
    contains="BiodbXmlEntry"
)
