#' Mass spectra SQLite entry class.
#'
#' This is the entry class for a Mass spectra SQLite database.
#'
#' @seealso Super class \code{\link{BiodbListEntry}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::newInst()
#'
#' # Get path to LCMS database example file
#' lcmsdb <- system.file("extdata", "massbank_extract.sqlite", package="biodb")
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
#' @export
MassSqliteEntry <- R6::R6Class("MassSqliteEntry",
inherit=BiodbListEntry
)
