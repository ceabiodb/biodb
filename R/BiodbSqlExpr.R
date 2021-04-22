#' The SQL Expression abstact class.
#'
#' This abstract class represents an SQL expression.
#'
#' @import R6
BiodbSqlExpr <- R6::R6Class("BiodbSqlExpr",

public=list(

#' @description
#' Converts into a string.
#' @return A string containing the SQL expression.
toString=function() {
    stop("This method is abstract.")
}
),

private=list(
))
