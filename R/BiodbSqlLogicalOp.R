#' This class represents an SQL logical operator.
#'
#' @include BiodbSqlExpr.R
BiodbSqlLogicalOp <- R6::R6Class("BiodbSqlLogicalOp",
inherit=BiodbSqlExpr,

public=list(

initialize=function(op) {
    private$op <- op
    private$expr <- list()
},

addExpr=function(expr) {
    private$expr <- c(private$expr, expr)
},

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
