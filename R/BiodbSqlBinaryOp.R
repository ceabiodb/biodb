#' This class represents an SQL binary operator.
#'
#' @import R6
#' @include BiodbSqlExpr.R
BiodbSqlBinaryOp <- R6::R6Class("BiodbSqlBinaryOp",
inherit=BiodbSqlExpr,

public=list(

initialize=function(lexpr, op, rexpr) {
    private$op <- op
    private$lexpr <- lexpr
    private$rexpr <- rexpr
},

toString=function() {
    s <- paste0('(', private$lexpr$toString(), ' ', private$op, ' ' ,
                private$rexpr$toString(), ')')
    return(s)
}
),

private=list(
    op=NULL,
    lexpr=NULL,
    rexpr=NULL
))
