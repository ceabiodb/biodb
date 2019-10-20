# vi: fdm=marker ts=4 et cc=80 tw=80

# MirbaseMatureEntry {{{1
################################################################################

# Declaration {{{2
################################################################################

#' miRBase Mature entry class.
#'
#' This is the connector class for Mirbase Mature database.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('mirbase.mature')
#'
#' # Get an entry
#' \dontrun{
#' e <- conn$getEntry('MIMAT0000433')
#' }
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include BiodbTxtEntry.R
#' @export MirbaseMatureEntry
#' @exportClass MirbaseMatureEntry
MirbaseMatureEntry <- methods::setRefClass("MirbaseMatureEntry",
    contains="BiodbTxtEntry")
