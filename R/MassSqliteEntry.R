# vi: fdm=marker ts=4 et cc=80 tw=80

# MassSqliteEntry {{{1
################################################################################

# Declaration {{{2
################################################################################

#' Mass SQLite entry class.
#'
#' This is the entry class for a MASS SQLite database.
#'
#' @seealso Super class \code{\link{BiodbListEntry}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Get path to LCMS database example file
#' lcmsdb <- system.file("extdata", "lcmsdb.sqlite", package="biodb")
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('mass.sqlite', url=lcmsdb)
#'
#' # Get an entry
#' e <- conn$getEntry('34.pos.col12.0.78')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include BiodbListEntry.R
#' @export MassSqliteEntry
#' @exportClass MassSqliteEntry
MassSqliteEntry <- methods::setRefClass("MassSqliteEntry",
    contains="BiodbListEntry")
