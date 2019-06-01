# vi: fdm=marker ts=4 et cc=80

# SQL Query class {{{1
################################################################################

# Class declaration {{{2
################################################################################

#' Class SQL Query.
#'
#' This class represents an SQL query. It is used internally to generate an SQL query string.
#'
#' @param table1        The first table of the join.
#' @param field1        The field of the first table of the join.
#' @param table2        The second table of the join.
#' @param field2        The field of the second table of the join.
#'
#' @seealso \code{\link{BiodbRequestScheduler}}, \code{\link{BiodbRequest}}.
#'
#' @import methods
#' @export BiodbSqlQuery
#' @exportClass BiodbSqlQuery
BiodbSqlQuery <- methods::setRefClass("BiodbSqlQuery", fields = list(.table = 'character', .fields = 'list', .distinct = 'logical', .join = 'list', .where = 'ANY', .limit = 'integer'))

# Initialize {{{2
################################################################################

BiodbSqlQuery$methods( initialize = function() {
    .self$.table <- character()
    .self$.fields <- list()
    .self$.distinct <- FALSE
    .self$.join <- list()
    .self$.where <- NULL
    .self$.limit <- as.integer(0)
})

# Add table {{{2
################################################################################

BiodbSqlQuery$methods( setTable = function(table) {
    "Set the table."

    .self$.table <- table
})

# Add  {{{2
################################################################################

BiodbSqlQuery$methods( addField = function(table = NULL, field) {
    "Set the fields."

    .self$.fields <- c(.self$.fields, list(list(table = table, field = field)))
})

# Set distinct {{{2
################################################################################

BiodbSqlQuery$methods( setDistinct = function(distinct) {
    "Set or unset distinct modifier."

    .self$.distinct <- as.logical(distinct)
})

# Set limit {{{2
################################################################################

BiodbSqlQuery$methods( setLimit = function(limit) {
    "Set results limit."

    .self$.limit <- as.integer(limit)
})

# Add join {{{2
################################################################################

BiodbSqlQuery$methods( addJoin = function(table1, field1, table2, field2) {
    "Add a join."

    # Check if this join already exists
    duplicate = any(vapply(.self$.join, function(x) ((x$table1 == table1 && x$field1 == field1 && x$table2 == table2 && x$field2 == field2) || (x$table1 == table1 && x$field1 == field1 && x$table2 == table2 && x$field2 == field2)), FUN.VALUE = TRUE))

    # Append
    if ( ! duplicate)
        .self$.join <- c(.self$.join, list(list(table1 = table1, field1 = field1, table2 = table2, field2 = field2)))
})

# Set where {{{2
################################################################################

BiodbSqlQuery$methods( setWhere = function(expr) {
    "Set  the where clause."

    .self$.where <- expr
})

# Get join {{{2
################################################################################

BiodbSqlQuery$methods( getJoin = function() {

    join = character()

    for (j in .self$.join)
        join = c(join, 'join', paste0('`', j$table1, '`'), 'on', paste0('`', j$table1, '`.`', j$field1, '`'), '=', paste0('`', j$table2, '`.`', j$field2, '`'))

    return(join)
})

# Get where {{{2
################################################################################

BiodbSqlQuery$methods( getWhere = function() {
    return(.self$.where)
})

# Get fields {{{2
################################################################################

BiodbSqlQuery$methods( getFields = function() {

    fields = vapply(.self$.fields, function(x) { field = (if (x$field == '*') x$field else paste0('`', x$field, '`')) ; if (is.null(x$table)) field else paste0('`', x$table, '`.', field) }, FUN.VALUE = '')

    fields = paste(fields, collapse = ', ')

    return(fields)
})

# To string {{{2
################################################################################

BiodbSqlQuery$methods( toString = function() {
    "Generate the string representation of this query."

    query = 'select'

    # Set distinct modifier
    if (.self$.distinct)
        query = c(query, 'distinct')

    # Set fields
    query = c(query, .self$getFields())

    # Set table
    query = c(query, 'from', paste0('`', .self$.table, '`'))

    # Set join clause
    query = c(query, .self$getJoin())

    # Set where clause
    if ( ! is.null(.self$.where)) {
        where = .self$.where$toString()
        if (nchar(where) > 0)
            query = c(query, 'where', where)
    }

    # Set limit
    if (.self$.limit > 0)
        query = c(query, 'limit', .self$.limit)

    # Join all strings
    query = paste(query, collapse = ' ')

    # End query
    query = paste0(query, ';')

    return(query)
})

# SQL expression {{{1
################################################################################

# Class declaration {{{2
################################################################################

BiodbSqlExpr = methods::setRefClass("BiodbSqlExpr", fields = list())

# Initialize {{{2
################################################################################

BiodbSqlExpr$methods( initialize = function() {
})

# To string {{{2
################################################################################

BiodbSqlExpr$methods( toString = function() {
    stop("This method is abstract.")
})

# SQL logical operator {{{1
################################################################################

# Class declaration {{{2
################################################################################

BiodbSqlLogicalOp <- methods::setRefClass("BiodbSqlLogicalOp", contains = 'BiodbSqlExpr', fields = list(.op = 'character', .expr = 'list'))

# Initialize {{{2
################################################################################

BiodbSqlLogicalOp$methods( initialize = function(op) {
    .self$.op <- op
    .self$.expr <- list()
})

# Add expression {{{2
################################################################################

BiodbSqlLogicalOp$methods( addExpr = function(expr) {
    .self$.expr <- c(.self$.expr, expr)
})

# To string {{{2
################################################################################

BiodbSqlLogicalOp$methods( toString = function() {
    s = vapply(.self$.expr, function(e) e$toString(), FUN.VALUE = '')
    s = s[vapply(s, function(x) nchar(x) > 0, FUN.VALUE = TRUE)]
    s = paste(s, collapse = paste0(' ', .self$.op, ' '))
    if (nchar(s) > 0)
        s = paste0('(', s, ')')
    return(s)
})

# SQL binary operator {{{1
################################################################################

# Class declaration {{{2
################################################################################

BiodbSqlBinaryOp <- methods::setRefClass("BiodbSqlBinaryOp", contains = "BiodbSqlExpr", fields = list(.op = 'character', .lexpr = 'BiodbSqlExpr', .rexpr = 'BiodbSqlExpr'))

# Initialize {{{2
################################################################################

BiodbSqlBinaryOp$methods( initialize = function(lexpr, op, rexpr) {
    .self$.op <- op
    .self$.lexpr <- lexpr
    .self$.rexpr <- rexpr
})

# To string {{{2
################################################################################

BiodbSqlBinaryOp$methods( toString = function() {
    s = paste0('(', .self$.lexpr$toString(), ' ', .self$.op, ' ' , .self$.rexpr$toString(), ')')
    return(s)
})

# SQL value {{{1
################################################################################

# Class declaration {{{2
################################################################################

BiodbSqlValue <- methods::setRefClass("BiodbSqlValue", contains = "BiodbSqlExpr", fields = list(.value = 'ANY'))

# Initialize {{{2
################################################################################

BiodbSqlValue$methods( initialize = function(value) {
    .self$.value <- value
})

# To string {{{2
################################################################################

BiodbSqlValue$methods( toString = function() {

    # Quote strings
    if (is.character(.self$.value))
        s = paste0('"', .self$.value, '"')
    else
        s = as.character(.self$.value)

    return(s)
})

# SQL list of values {{{1
################################################################################

# Class declaration {{{2
################################################################################

BiodbSqlList <- methods::setRefClass("BiodbSqlList", contains = "BiodbSqlExpr", fields = list(.values = 'ANY'))

# Initialize {{{2
################################################################################

BiodbSqlList$methods( initialize = function(values) {
    .self$.values <- values
})

# To string {{{2
################################################################################

BiodbSqlList$methods( toString = function() {

    # Quote strings
    if (is.character(.self$.values))
        s = vapply(.self$.values, function(v) paste0('"', v, '"'), FUN.VALUE = '')

    # Collapse and convert to string
    s = paste0('(', paste(s, collapse = ', '), ')')

    return(s)
})

# SQL field {{{1
################################################################################

# Class declaration {{{2
################################################################################

BiodbSqlField <- methods::setRefClass("BiodbSqlField", contains = "BiodbSqlExpr", fields = list(.table = 'character', .field = 'character'))

# Initialize {{{2
################################################################################

BiodbSqlField$methods( initialize = function(table = NA_character_, field) {
    .self$.table <- table
    .self$.field <- field
})

# To string {{{2
################################################################################

BiodbSqlField$methods( toString = function() {
    s = paste0('`', .self$.field, '`')
    if ( ! is.na(.self$.table))
        s = paste0('`', .self$.table, '`.', s)
    return(s)
})
