#' Compound CSV File connector class.
#'
#' This is the connector class for a Compound CSV file database.
#'
#' @seealso Super class \code{\link{CsvFileConn}} and interfaces
#' \code{\link{BiodbCompounddbConn}}, \code{\link{BiodbWritable}} and
#' \code{\link{BiodbEditable}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Get a connector:
#' chebi_file <- system.file("extdata", "chebi_extract.tsv", package="biodb")
#' conn <- mybiodb$getFactory()$createConn('comp.csv.file', url=chebi_file)
#'
#' # Get an entry
#' e <- conn$getEntry('')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include CsvFileConn.R
#' @include BiodbCompounddbConn.R
#' @include BiodbEditable.R
#' @include BiodbWritable.R
#' @export CompCsvFileConn
#' @exportClass CompCsvFileConn
CompCsvFileConn <- methods::setRefClass("CompCsvFileConn",
    contains=c("CsvFileConn", "BiodbCompounddbConn", 'BiodbWritable',
               'BiodbEditable'),
    fields=list(
        ),

methods=list(

initialize=function(...) {

    callSuper(...)
},

.doSelect=function(db) {
    return(db)
}
))
