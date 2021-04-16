#' Compound CSV File connector class.
#'
#' This is the connector class for a Compound CSV file database.
#'
#' @seealso Super class \code{\link{CsvFileConn}} and interfaces
#' \code{\link{BiodbCompounddbConn}}.
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
#' @export CompCsvFileConn
#' @exportClass CompCsvFileConn
CompCsvFileConn <- methods::setRefClass("CompCsvFileConn",
    contains=c("CsvFileConn", "BiodbCompounddbConn"),
    fields=list(),

methods=list(

initialize=function(...) {

    callSuper(...)
},

.doSelect=function(db) {
    return(db)
},

.doSearchForEntries=function(fields=NULL, max.results=0) {
    # To be implemented by derived class.
    
    db <- NULL
    ids <- character()

    # Search by name
    if ( 'name' %in% names(fields))
        db <- .self$.selectBySubstring(db, 'name', fields$name)

    # Search by mass
    ef <- .self$getBiodb()$getEntryFields()
    for (field in names(fields)) {
        if (ef$get(field)$getType() == 'mass') {
            param <- fields[[field]]
            if ('min' %in% names(param)) {
                .self$.checkMassField(mass=param$min, mass.field=field)
                rng <- list(a=param$min, b=param$max)
            }
            else {
                .self$.checkMassField(mass=param$value, mass.field=field)
                if ('delta' %in% names(param))
                    rng <- convertTolToRange(param$value, param$delta, 'delta')
                else
                    rng <- convertTolToRange(param$value, param$ppm, 'ppm')
            }
            db <- .self$.selectByRange(db=db, field=field,
                                       minValue=rng$a, maxValue=rng$b)
        }
    }
    
    # Get IDs
    if ( ! is.null(db))
        ids <- .self$.select(db=db, cols='accession', drop=TRUE, uniq=TRUE)

    return(ids)
}
))
