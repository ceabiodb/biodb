#' This class represents an SQL logical operator.
#'
#' @import R6
#' @include BiodbSqlExpr.R
BiodbSqlLogicalOp <- R6::R6Class("BiodbSqlLogicalOp",
inherit=BiodbSqlExpr,

public=list(

#' @description
#' Initializer.
#' @param op The logical operator, as a string.
#' @return Nothing.
initialize=function(op) {
    private$op <- op
    private$expr <- list()

    return(invisible(NULL))
},

#' @description
#' Add an SQL expression to the logical operator.
#' @param expr A BiodbSqlExpr instance.
#' @return Nothing.
addExpr=function(expr) {
    private$expr <- c(private$expr, expr)

    return(invisible(NULL))
},

#' @description
#' Converts into a string.
#' @return A string containing the SQL expression.
toString=function() {
    s <- vapply(private$expr, function(e) e$toString(), FUN.VALUE='')
    s <- s[vapply(s, function(x) nchar(x) > 0, FUN.VALUE=TRUE)]
    s <- paste(s, collapse=paste0(' ', private$op, ' '))
    if (nchar(s) > 0)
        s <- paste0('(', s, ')')
    return(s)
}
),

private=list(
    op=NULL,
    expr=NULL
))
