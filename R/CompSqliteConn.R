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
CompSqliteConn <- methods::setRefClass('CompSqliteConn',
    contains=c("SqliteConn", "BiodbCompounddbConn"),
    fields=list(
    ),

methods=list(

.doSearchForEntries=function(fields=NULL, max.results=0) {
    # Overrides super class' method.

    ids <- character()
    .self$.initDb()

    print('-------------------------------- CompSqliteConn::.doSearchForEntries 10')
    if ( ! is.null(.self$.db)) {
    print('-------------------------------- CompSqliteConn::.doSearchForEntries 11')

        query <- BiodbSqlQuery()
        query$setTable('entries')
        query$setDistinct(TRUE)
        query$setWhere(BiodbSqlLogicalOp(op='and'))
    print('-------------------------------- CompSqliteConn::.doSearchForEntries 15')

        # Search by name
        if ('name' %in% names(fields)) {
            query$addJoin(table1='name', field1='accession',
                          table2='entries', field2='accession')
            expr <- BiodbSqlBinaryOp(lexpr=BiodbSqlField(table='name',
                                                         field='name'), op='=',
                                     rexpr=BiodbSqlValue(fields$name))
            query$getWhere()$addExpr(expr)
        }
    print('-------------------------------- CompSqliteConn::.doSearchForEntries 20')
        
        # Search by mass
        ef <- .self$getBiodb()$getEntryFields()
        for (field in names(fields)) {
    print('-------------------------------- CompSqliteConn::.doSearchForEntries 30')
            
            # This is a mass field
            if (ef$get(field)$getType() == 'mass') {
                param <- fields[[field]]
                
                # Compute range
                if ('min' %in% names(param)) {
    print('-------------------------------- CompSqliteConn::.doSearchForEntries 40')
                    .self$.checkMassField(mass=param$min, mass.field=field)
                    rng <- list(a=param$min, b=param$max)
                }
                else {
    print('-------------------------------- CompSqliteConn::.doSearchForEntries 50')
                    .self$.checkMassField(mass=param$value, mass.field=field)
                    if ('delta' %in% names(param))
                        rng <- convertTolToRange(param$value, param$delta,
                                                 'delta')
                    else
                        rng <- convertTolToRange(param$value, param$ppm, 'ppm')
                }
                
    print('-------------------------------- CompSqliteConn::.doSearchForEntries 60')
                # Complete query
                expr <- BiodbSqlBinaryOp(lexpr=BiodbSqlField(table="entries",
                                                              field=field),
                                          op='>=', rexpr=BiodbSqlValue(rng$a))
                query$getWhere()$addExpr(expr)
                expr <- BiodbSqlBinaryOp(lexpr=BiodbSqlField(table="entries",
                                                             field=field),
                                         op='<=', rexpr=BiodbSqlValue(rng$b))
                query$getWhere()$addExpr(expr)
    print('-------------------------------- CompSqliteConn::.doSearchForEntries 70')
            }
        }
        
    print('-------------------------------- CompSqliteConn::.doSearchForEntries 80')
        # Cut
        if (max.results > 0)
            query$setLimit(max.results)
    print('-------------------------------- CompSqliteConn::.doSearchForEntries 90')
        
        # Run query
        query$addField(table="entries", field='accession')
        x <- .self$getQuery(query)
        ids <- x[[1]]
    print('-------------------------------- CompSqliteConn::.doSearchForEntries 100')
    }
    print('-------------------------------- CompSqliteConn::.doSearchForEntries 110')
    
    return(ids)
}
))
