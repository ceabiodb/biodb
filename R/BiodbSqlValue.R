#' This class represents an SQL value.
#'
#' @include BiodbSqlExpr.R
BiodbSqlValue <- R6::R6Class("BiodbSqlValue",
inherit=BiodbSqlExpr,

public=list(

initialize=function(value) {
    private$value <- value
},

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
