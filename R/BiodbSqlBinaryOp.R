#' This class represents an SQL binary operator.
#'
#' @import R6
#' @include BiodbSqlExpr.R
BiodbSqlBinaryOp <- R6::R6Class("BiodbSqlBinaryOp",
inherit=BiodbSqlExpr,

public=list(

#' @description
#' Initializer.
#' @param lexpr A BiodbSqlExpr instance for the left part.
#' @param op The binary operator, as a string.
#' @param rexpr A BiodbSqlExpr instance for the right part.
#' @return Nothing.
initialize=function(lexpr, op, rexpr) {
    chk::chk_is(lexpr, 'BiodbSqlExpr')
    chk::chk_is(rexpr, 'BiodbSqlExpr')
    chk::chk_string(op)

    private$op <- op
    private$lexpr <- lexpr
    private$rexpr <- rexpr

    return(invisible(NULL))
},

#' @description
#' Converts into a string.
#' @return A string containing the SQL expression.
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
