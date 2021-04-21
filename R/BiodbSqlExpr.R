#' This abstract class represents an SQL expression.
BiodbSqlExpr <- R6::R6Class("BiodbSqlExpr",

public=list(

toString=function() {
    stop("This method is abstract.")
}
),

private=list(
))
