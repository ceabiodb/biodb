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
},

searchCompound=function(name=NULL, mass=NULL, mass.field=NULL,
                        mass.tol=0.01, mass.tol.unit='plain',
                        max.results=NA_integer_) {
    # Overrides super class' method.

    .self$.checkMassField(mass=mass, mass.field=mass.field)
    
    db <- NULL
    ids <- character()

    # Search for name
    if ( ! is.null(name))
        db <- .self$.selectBySubstring(db, 'name', name)

    # Search for mass
    if ( ! is.null(mass)) {
        rng <- convertTolToRange(mass, mass.tol, mass.tol.unit)
        db <- .self$.selectByRange(db=db, field=mass.field,
                                   minValue=rng$a, maxValue=rng$b)
    }
    
    if ( ! is.null(db))
        ids <- .self$.select(db=db, cols='accession', drop=TRUE, uniq=TRUE)
    
    return(ids)
}
))
