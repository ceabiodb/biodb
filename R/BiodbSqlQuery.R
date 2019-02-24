# vi: fdm=marker

# Class declaration {{{1
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
BiodbSqlQuery <- methods::setRefClass("BiodbSqlQuery", fields = list(.table = 'character', .fields = 'list', .distinct = 'logical', .join = 'list', .where = 'list', .limit = 'integer'))

# Constructor {{{1
################################################################

BiodbSqlQuery$methods( initialize = function() {
	.table <<- character()
	.fields <<- list()
	.distinct <<- FALSE
	.join <<- list()
	.where <<- list()
	.limit <<- as.integer(0)
})

# Add table {{{1
################################################################

BiodbSqlQuery$methods( setTable = function(table) {
	":\n\nSet the table."

	.table <<- table
})

# Add  {{{1
################################################################

BiodbSqlQuery$methods( addField = function(table = NULL, field) {
	":\n\nSet the fields."

	.fields <<- c(.self$.fields, list(list(table = table, field = field)))
})

# Set distinct {{{1
################################################################

BiodbSqlQuery$methods( setDistinct = function(distinct) {
	":\n\nSet or unset distinct modifier."

	.distinct <<- as.logical(distinct)
})

# Set limit {{{1
################################################################

BiodbSqlQuery$methods( setLimit = function(limit) {
	":\n\nSet results limit."

	.limit <<- as.integer(limit)
})

# Add join {{{1
################################################################

BiodbSqlQuery$methods( addJoin = function(table1, field1, table2, field2) {
	":\n\nAdd a join."

	# Check if this join already exists
	duplicate = any(vapply(.self$.join, function(x) ((x$table1 == table1 && x$field1 == field1 && x$table2 == table2 && x$field2 == field2) || (x$table1 == table1 && x$field1 == field1 && x$table2 == table2 && x$field2 == field2)), FUN.VALUE = TRUE))

	# Append
	if ( ! duplicate)
		.join <<- c(.self$.join, list(list(table1 = table1, field1 = field1, table2 = table2, field2 = field2)))
})

# Add where {{{1
################################################################

BiodbSqlQuery$methods( addWhere = function(op, table1, field1, table2 = NULL, field2 = NULL, value2 = NULL) {
	":\n\nAdd a condition for the where statement."

	.where <<- c(.self$.where, list(list(table1 = table1, field1 = field1, table2 = table2, field2 = field2, value2 = value2, op = op)))
})

# Get join {{{1
################################################################

BiodbSqlQuery$methods( getJoin = function() {

	join = character()

	for (j in .self$.join)
		join = c(join, 'join', paste0('`', j$table1, '`'), 'on', paste0('`', j$table1, '`.`', j$field1, '`'), '=', paste0('`', j$table2, '`.`', j$field2, '`'))

	return(join)
})

# Get where {{{1
################################################################

BiodbSqlQuery$methods( getWhere = function() {

	where = 'where'

	i = 0
	for (w in .self$.where) {
		i = i + 1
		if (i > 1)
			where = c(where, 'and')

		# Set left member
		left.member = paste0('`', w$table1, '`.`', w$field1, '`')

		# Set right member
		if ( ! is.null(w$value2))
			right.member = if (is.character(w$value2)) paste0('"', w$value2, '"') else w$value2
		else
			right.member = paste0('`', w$table2, '`.`', w$field2, '`')

		where = c(where, left.member, w$op, right.member)
	}

	return(where)
})

# Get fields {{{1
################################################################

BiodbSqlQuery$methods( getFields = function() {

	fields = vapply(.self$.fields, function(x) { field = (if (x$field == '*') x$field else paste0('`', x$field, '`')) ; if (is.null(x$table)) field else paste0('`', x$table, '`.', field) }, FUN.VALUE = '')

	fields = paste(fields, collapse = ', ')

	return(fields)
})

# To string {{{1
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
	query = c(query, .self$getWhere())

	# Set limit
	if (.self$.limit > 0)
		query = c(query, 'limit', .self$.limit)

	# Join all strings
	query = paste(query, collapse = ' ')

	# End query
	query = paste0(query, ';')

	return(query)
})
