#' This class handles an SQL Query.
#'
#' This class represents an SQL query. It is used internally to generate an SQL
#' query string.
#'
#' @param table1        The first table of the join.
#' @param field1        The field of the first table of the join.
#' @param table2        The second table of the join.
#' @param field2        The field of the second table of the join.
#'
#' @seealso \code{\link{BiodbRequestScheduler}}, \code{\link{BiodbRequest}}.
#'
#' @import methods
BiodbSqlQuery <- methods::setRefClass("BiodbSqlQuery",
    fields=list(
        .table='character',
        .fields='list',
        .distinct='logical',
        .join='list',
        .where='ANY',
        .limit='integer'),

methods=list(

initialize=function() {
    .self$.table <- character()
    .self$.fields <- list()
    .self$.distinct <- FALSE
    .self$.join <- list()
    .self$.where <- NULL
    .self$.limit <- as.integer(0)
},

setTable=function(table) {
    "Set the table."

    .self$.table <- table
},

addField=function(table=NULL, field) {
    "Set the fields."

    .self$.fields <- c(.self$.fields, list(list(table=table, field=field)))
},

setDistinct=function(distinct) {
    "Set or unset distinct modifier."

    .self$.distinct <- as.logical(distinct)
},

setLimit=function(limit) {
    "Set results limit."

    .self$.limit <- as.integer(limit)
},

addJoin=function(table1, field1, table2, field2) {
    "Add a join."

    # Check if this join already exists
    fct <- function(x) ((x$table1 == table1 && x$field1 == field1
                         && x$table2 == table2 && x$field2 == field2)
        || (x$table1 == table1 && x$field1 == field1 && x$table2 == table2
            && x$field2 == field2))
    duplicate <- any(vapply(.self$.join, fct, FUN.VALUE=TRUE))

    # Append
    if ( ! duplicate) {
        lst <- list(table1=table1, field1=field1, table2=table2, field2=field2)
        .self$.join <- c(.self$.join, list(lst))
    }
},

setWhere=function(expr) {
    "Set  the where clause."

    .self$.where <- expr
},

getJoin=function() {

    join <- character()

    for (j in .self$.join) {
        j1 <- paste(DBI::dbQuoteIdentifier(DBI::ANSI(), j$table1),
                    DBI::dbQuoteIdentifier(DBI::ANSI(), j$field1), sep='.')
        j2 <- paste(DBI::dbQuoteIdentifier(DBI::ANSI(), j$table2),
                    DBI::dbQuoteIdentifier(DBI::ANSI(), j$field2), sep='.')
        join <- c(join, 'join', DBI::dbQuoteIdentifier(DBI::ANSI(), j$table1),
                  'on', j1, '=', j2)
    }

    return(join)
},

getWhere=function() {
    return(.self$.where)
},

getFields=function() {

    fct <- function(x) {
        field <- DBI::dbQuoteIdentifier(DBI::ANSI(), x$field)
        if (is.null(x$table))
            field
        else
            paste(DBI::dbQuoteIdentifier(DBI::ANSI(), x$table), field, sep='.')
    }

    fields <- vapply(.self$.fields, fct, FUN.VALUE='')

    fields=paste(fields, collapse=', ')

    return(fields)
},

toString=function() {
    "Generates the string representation of this query."

    query <- 'select'

    # Set distinct modifier
    if (.self$.distinct)
        query <- c(query, 'distinct')

    # Set fields
    query <- c(query, .self$getFields())

    # Set table
    query <- c(query, 'from', DBI::dbQuoteIdentifier(DBI::ANSI(), .self$.table))

    # Set join clause
    query <- c(query, .self$getJoin())

    # Set where clause
    if ( ! is.null(.self$.where)) {
        where <- .self$.where$toString()
        if (nchar(where) > 0)
            query <- c(query, 'where', where)
    }

    # Set limit
    if (.self$.limit > 0)
        query <- c(query, 'limit', .self$.limit)

    # Join all strings
    query <- paste(query, collapse=' ')

    # End query
    query <- paste0(query, ';')

    return(query)
}

))
