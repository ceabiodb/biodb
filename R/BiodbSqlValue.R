#' This class represents an SQL value.
#'
#' @import R6
#' @include BiodbSqlExpr.R
BiodbSqlValue <- R6::R6Class("BiodbSqlValue",
inherit=BiodbSqlExpr,

public=list(

#' @description
#' Initializer.
#' @param value The value.
#' @return Nothing.
initialize=function(value) {
    private$value <- value

    return(invisible(NULL))
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
