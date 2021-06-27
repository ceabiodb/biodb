#' Compound CSV File entry class.
#'
#' This is the entry class for Compound CSV file databases.
#'
#' @seealso Super class \code{\link{BiodbCsvEntry}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::newInst()
#'
#' # Get a connector that inherits from CsvFileConn:
#' chebi_file <- system.file("extdata", "chebi_extract.tsv", package="biodb")
#' conn <- mybiodb$getFactory()$createConn('comp.csv.file', url=chebi_file)
#'
#' # Get an entry
#' e <- conn$getEntry('')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include BiodbCsvEntry.R
#' @export
CompCsvFileEntry <- R6::R6Class("CompCsvFileEntry",
inherit=BiodbCsvEntry,


public=list(

initialize=function(...) {

    super$initialize(sep="\t", ...)
}
),

private=list(
))
