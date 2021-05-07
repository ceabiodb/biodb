#' This class handles an SQL Query.
#'
#' This class represents an SQL query. It is used internally to generate an SQL
#' query string.
#'
#' @seealso \code{\link{BiodbRequestScheduler}}, \code{\link{BiodbRequest}}.
#'
#' @import R6
BiodbSqlQuery <- R6::R6Class("BiodbSqlQuery",

public=list(

#' @description
#' Constructor.
#' @return A new instance.
initialize=function() {
    private$table <- character()
    private$fields <- list()
    private$distinct <- FALSE
    private$join <- list()
    private$where <- NULL
    private$limit <- as.integer(0)
},

#' @description
#' Set the table.
#' @param table The table name.
#' @return Nothing.
setTable=function(table) {
    private$table <- table
},

#' @description
#' Set the fields.
#' @param table The table name.
#' @param field A field name.
#' @return Nothing.
addField=function(table=NULL, field) {
    private$fields <- c(private$fields, list(list(table=table, field=field)))
},

#' @description
#' Set or unset distinct modifier.
#' @param distinct Either TRUE or FALSE for setting or unsetting the distinct
#' flag.
#' @return Nothing.
setDistinct=function(distinct) {
    chk::chk_flag(distinct)
    private$distinct <- as.logical(distinct)
},

#' @description
#' Set results limit.
#' @param limit The limit to set, as an integer value.
#' @return Nothing.
setLimit=function(limit) {
    chk::chk_whole_number(limit)
    private$limit <- as.integer(limit)
},

#' @description
#' Add a join.
#' @param table1 The first table.
#' @param field1 The field of the first table.
#' @param table2 The second table.
#' @param field2 The field of the second table.
#' @return Nothing.
addJoin=function(table1, field1, table2, field2) {
    # Check if this join already exists
    fct <- function(x) ((x$table1 == table1 && x$field1 == field1
        && x$table2 == table2 && x$field2 == field2)
        || (x$table1 == table1 && x$field1 == field1 && x$table2 == table2
        && x$field2 == field2))
    duplicate <- any(vapply(private$join, fct, FUN.VALUE=TRUE))

    # Append
    if ( ! duplicate) {
        lst <- list(table1=table1, field1=field1, table2=table2, field2=field2)
        private$join <- c(private$join, list(lst))
    }
},

#' @description
#' Set the where clause.
#' @param expr A BiodbSqlExpr representing the "where" clause.
#' @return Nothing.
setWhere=function(expr) {
    private$where <- expr
},

#' @description
#' Builds and returns the join expression.
#' @return A character vector representing the join expression.
getJoin=function() {

    join <- character()

    for (j in private$join) {
        j1 <- paste(DBI::dbQuoteIdentifier(DBI::ANSI(), j$table1),
            DBI::dbQuoteIdentifier(DBI::ANSI(), j$field1), sep='.')
        j2 <- paste(DBI::dbQuoteIdentifier(DBI::ANSI(), j$table2),
            DBI::dbQuoteIdentifier(DBI::ANSI(), j$field2), sep='.')
        join <- c(join, 'join', DBI::dbQuoteIdentifier(DBI::ANSI(), j$table1),
            'on', j1, '=', j2)
    }

    return(join)
},

#' @description
#' Gets the where expression.
#' @return The BiodbSqlExpr instance representing the "where" clause.
getWhere=function() {
    return(private$where)
},

#' @description
#' Gets the fields to retrieve.
#' @return A string containing the list of fields to retrieve.
getFields=function() {

    fct <- function(x) {
        field <- DBI::dbQuoteIdentifier(DBI::ANSI(), x$field)
        if (is.null(x$table))
            field
        else
            paste(DBI::dbQuoteIdentifier(DBI::ANSI(), x$table), field, sep='.')
    }

    fields <- vapply(private$fields, fct, FUN.VALUE='')

    fields=paste(fields, collapse=', ')

    return(fields)
},

#' @description
#' Generates the string representation of this query.
#' @return A string containing the full SQL query.
toString=function() {
    query <- 'select'

    # Set distinct modifier
    if (private$distinct)
        query <- c(query, 'distinct')

    # Set fields
    query <- c(query, self$getFields())

    # Set table
    query <- c(query, 'from',
        DBI::dbQuoteIdentifier(DBI::ANSI(), private$table))

    # Set join clause
    query <- c(query, self$getJoin())

    # Set where clause
    if ( ! is.null(private$where)) {
        where <- private$where$toString()
        if (nchar(where) > 0)
            query <- c(query, 'where', where)
    }

    # Set limit
    if (private$limit > 0)
        query <- c(query, 'limit', private$limit)

    # Join all strings
    query <- paste(query, collapse=' ')

    # End query
    query <- paste0(query, ';')

    return(query)
}
),

private=list(
    table=NULL,
    fields=NULL,
    distinct=NULL,
    join=NULL,
    where=NULL,
    limit=NULL
))
