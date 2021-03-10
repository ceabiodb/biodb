#' Class for handling a Compound database in SQLite format.
#'
#' This is the connector class for a Compound database.
#'
#' @seealso Super class \code{\link{SqliteConn}} and interfaces
#' \code{\link{BiodbCompounddbConn}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Get a connector:
#' chebi_file <- system.file("extdata", "chebi_extract.sqlite", package="biodb")
#' conn <- mybiodb$getFactory()$createConn('comp.sqlite', url=chebi_file)
#'
#' # Get an entry
#' e <- conn$getEntry('1018')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @import methods
#' @include SqliteConn.R
MassSqliteConn <- methods::setRefClass('MassSqliteConn',
    contains=c("SqliteConn", "BiodbCompounddbConn"),
    fields=list(
    ),

methods=list(
))
