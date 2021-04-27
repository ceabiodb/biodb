#' Compound SQLite entry class.
#'
#' This is the entry class for a Compound SQLite database.
#'
#' @seealso Super class \code{\link{BiodbListEntry}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::BiodbMain()
#'
#' # Get path to LCMS database example file
#' lcmsdb <- system.file("extdata", "chebi_extract.sqlite", package="biodb")
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('comp.sqlite', url=lcmsdb)
#'
#' # Get an entry
#' e <- conn$getEntry('34.pos.col12.0.78')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include BiodbListEntry.R
#' @export CompSqliteEntry
#' @exportClass CompSqliteEntry
CompSqliteEntry <- methods::setRefClass("CompSqliteEntry",
                                        contains="BiodbListEntry")
