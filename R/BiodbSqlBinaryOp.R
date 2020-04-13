#' This class represents an SQL binary operator.
#'
#' @include BiodbSqlExpr.R
BiodbSqlBinaryOp <- methods::setRefClass("BiodbSqlBinaryOp",
    contains="BiodbSqlExpr",
    fields=list(
        .op='character',
        .lexpr='BiodbSqlExpr',
        .rexpr='BiodbSqlExpr'
        ),

methods=list(

initialize=function(lexpr, op, rexpr) {
    .self$.op <- op
    .self$.lexpr <- lexpr
    .self$.rexpr <- rexpr
},

toString=function() {
    s <- paste0('(', .self$.lexpr$toString(), ' ', .self$.op, ' ' ,
                .self$.rexpr$toString(), ')')
    return(s)
}

))
