# vi: fdm=marker

# SQL Query class {{{1
################################################################

# Class declaration {{{2
################################################################

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

# Constructor {{{2
################################################################

BiodbSqlQuery$methods( initialize = function() {
	.table <<- character()
	.fields <<- list()
	.distinct <<- FALSE
	.join <<- list()
	.where <<- NULL
	.limit <<- as.integer(0)
})

# Add table {{{2
################################################################

BiodbSqlQuery$methods( setTable = function(table) {
	":\n\nSet the table."

	.table <<- table
})

# Add  {{{2
################################################################

BiodbSqlQuery$methods( addField = function(table = NULL, field) {
	":\n\nSet the fields."

	.fields <<- c(.self$.fields, list(list(table = table, field = field)))
})

# Set distinct {{{2
################################################################

BiodbSqlQuery$methods( setDistinct = function(distinct) {
	":\n\nSet or unset distinct modifier."

	.distinct <<- as.logical(distinct)
})

# Set limit {{{2
################################################################

BiodbSqlQuery$methods( setLimit = function(limit) {
	":\n\nSet results limit."

	.limit <<- as.integer(limit)
})

# Add join {{{2
################################################################

BiodbSqlQuery$methods( addJoin = function(table1, field1, table2, field2) {
	":\n\nAdd a join."

	# Check if this join already exists
	duplicate = any(vapply(.self$.join, function(x) ((x$table1 == table1 && x$field1 == field1 && x$table2 == table2 && x$field2 == field2) || (x$table1 == table1 && x$field1 == field1 && x$table2 == table2 && x$field2 == field2)), FUN.VALUE = TRUE))

	# Append
	if ( ! duplicate)
		.join <<- c(.self$.join, list(list(table1 = table1, field1 = field1, table2 = table2, field2 = field2)))
})

# Set where {{{2
################################################################

BiodbSqlQuery$methods( setWhere = function(expr) {
	":\n\nSet  the where clause."

	.where <<- expr
})

# Get join {{{2
################################################################

BiodbSqlQuery$methods( getJoin = function() {

	join = character()

	for (j in .self$.join)
		join = c(join, 'join', paste0('`', j$table1, '`'), 'on', paste0('`', j$table1, '`.`', j$field1, '`'), '=', paste0('`', j$table2, '`.`', j$field2, '`'))

	return(join)
})

# Get where {{{2
################################################################

BiodbSqlQuery$methods( getWhere = function() {
	return(.self$.where)
})

# Get fields {{{2
################################################################

BiodbSqlQuery$methods( getFields = function() {

	fields = vapply(.self$.fields, function(x) { field = (if (x$field == '*') x$field else paste0('`', x$field, '`')) ; if (is.null(x$table)) field else paste0('`', x$table, '`.', field) }, FUN.VALUE = '')

	fields = paste(fields, collapse = ', ')

	return(fields)
})

# To string {{{2
################################################################

BiodbSqlQuery$methods( toString = function() {
	":\n\nGenerate the string representation of this query."

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
################################################################

# Class declaration {{{2
################################################################

BiodbSqlExpr = methods::setRefClass("BiodbSqlExpr", fields = list())

# Constructor {{{2
################################################################

BiodbSqlExpr$methods( initialize = function() {
})

# To string {{{2
################################################################

BiodbSqlExpr$methods( toString = function() {
	stop("This method is abstract.")
})

# SQL logical operator {{{1
################################################################

# Class declaration {{{2
################################################################

BiodbSqlLogicalOp <- methods::setRefClass("BiodbSqlLogicalOp", contains = 'BiodbSqlExpr', fields = list(.op = 'character', .expr = 'list'))

# Constructor {{{2
################################################################

BiodbSqlLogicalOp$methods( initialize = function(op) {
	.op <<- op
	.expr <<- list()
})

# Add expression {{{2
################################################################

BiodbSqlLogicalOp$methods( addExpr = function(expr) {
	.expr <<- c(.self$.expr, expr)
})

# To string {{{2
################################################################

BiodbSqlLogicalOp$methods( toString = function() {
	s = vapply(.self$.expr, function(e) e$toString(), FUN.VALUE = '')
	s = s[vapply(s, function(x) nchar(x) > 0, FUN.VALUE = TRUE)]
	s = paste(s, collapse = paste0(' ', .self$.op, ' '))
	if (nchar(s) > 0)
		s = paste0('(', s, ')')
	return(s)
})

# SQL binary operator {{{1
################################################################

# Class declaration {{{2
################################################################

BiodbSqlBinaryOp <- methods::setRefClass("BiodbSqlBinaryOp", contains = "BiodbSqlExpr", fields = list(.op = 'character', .lexpr = 'BiodbSqlExpr', .rexpr = 'BiodbSqlExpr'))

# Constructor {{{2
################################################################

BiodbSqlBinaryOp$methods( initialize = function(lexpr, op, rexpr) {
	.op <<- op
	.lexpr <<- lexpr
	.rexpr <<- rexpr
})

# To string {{{2
################################################################

BiodbSqlBinaryOp$methods( toString = function() {
	s = paste0('(', .self$.lexpr$toString(), ' ', .self$.op, ' ' , .self$.rexpr$toString(), ')')
	return(s)
})

# SQL value {{{1
################################################################

# Class declaration {{{2
################################################################

BiodbSqlValue <- methods::setRefClass("BiodbSqlValue", contains = "BiodbSqlExpr", fields = list(.value = 'ANY'))

# Constructor {{{2
################################################################

BiodbSqlValue$methods( initialize = function(value) {
	.value <<- value
})

# To string {{{2
################################################################

BiodbSqlValue$methods( toString = function() {
	s = if (is.character(.self$.value)) paste0('"', .self$.value, '"') else paste(.self$.value)
	return(s)
})

# SQL field {{{1
################################################################

# Class declaration {{{2
################################################################

BiodbSqlField <- methods::setRefClass("BiodbSqlField", contains = "BiodbSqlExpr", fields = list(.table = 'character', .field = 'character'))

# Constructor {{{2
################################################################

BiodbSqlField$methods( initialize = function(table = NA_character_, field) {
	.table <<- table
	.field <<- field
})

# To string {{{2
################################################################

BiodbSqlField$methods( toString = function() {
	s = paste0('`', .self$.field, '`')
	if ( ! is.na(.self$.table))
		s = paste0('`', .self$.table, '`.', s)
	return(s)
})
