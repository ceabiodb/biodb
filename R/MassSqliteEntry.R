#' Mass spectra SQLite entry class.
#'
#' This is the entry class for a Mass spectra SQLite database.
#'
#' @seealso Super class \code{\link{BiodbListEntry}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::BiodbMain()
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
#' @export MassSqliteEntry
#' @exportClass MassSqliteEntry
MassSqliteEntry <- methods::setRefClass("MassSqliteEntry",
                                        contains="BiodbListEntry")
