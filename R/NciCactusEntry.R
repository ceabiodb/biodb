# vi: fdm=marker ts=4 et cc=80 tw=80

# NciCactusEntry {{{1
################################################################################

# Declaration {{{2
################################################################################

#' NCI CACTUS entry class.
#'
#' This is the entry class for the NCI CACTUS database.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('nci.cactus')
#'
#' # Get an entry
#' e <- conn$getEntry('749674')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include BiodbSdfEntry.R
#' @export NciCactusEntry 
#' @exportClass NciCactusEntry 
NciCactusEntry <- methods::setRefClass("NciCactusEntry",
    contains="BiodbSdfEntry")
