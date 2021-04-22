#' This class represents an SQL value.
#'
#' @import R6
#' @include BiodbSqlExpr.R
BiodbSqlValue <- R6::R6Class("BiodbSqlValue",
inherit=BiodbSqlExpr,

public=list(

#' @description
#' Constructor.
#' @param value The value.
#' @return A new instance.
initialize=function(value) {
    private$value <- value
},

#' @description
#' Converts into a string.
#' @return A string containing the SQL expression.
toString=function() {

    # Quote strings
    if (is.character(private$value))
        s <- paste0('"', private$value, '"')
    else
        s <- as.character(private$value)

    return(s)
}
),

private=list(
    value=NULL
))
