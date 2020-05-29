#' This class represents an SQL logical operator.
#'
#' @include BiodbSqlExpr.R
BiodbSqlLogicalOp <- methods::setRefClass("BiodbSqlLogicalOp",
    contains='BiodbSqlExpr',
    fields=list(
        .op='character',
        .expr='list'
        ),

methods=list(

initialize=function(op) {
    .self$.op <- op
    .self$.expr <- list()
},

addExpr=function(expr) {
    .self$.expr <- c(.self$.expr, expr)
},

toString=function() {
    s <- vapply(.self$.expr, function(e) e$toString(), FUN.VALUE='')
    s <- s[vapply(s, function(x) nchar(x) > 0, FUN.VALUE=TRUE)]
    s <- paste(s, collapse=paste0(' ', .self$.op, ' '))
    if (nchar(s) > 0)
        s <- paste0('(', s, ')')
    return(s)
}

))
