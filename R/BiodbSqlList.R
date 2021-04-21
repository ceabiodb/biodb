#' This class represents an SQL list.
#'
#' @include BiodbSqlExpr.R
BiodbSqlList <- R6::R6Class("BiodbSqlList",
inherit=BiodbSqlExpr,

public=list(

initialize=function(values) {
    private$values <- values
},

toString=function() {

    # Quote strings
    if (is.character(private$values)) {
        fct <- function(v) paste0('"', v, '"')
        s <- vapply(private$values, fct, FUN.VALUE='')
    }

    # Collapse and convert to string
    s <- paste0('(', paste(s, collapse=', '), ')')

    return(s)
}
),

private=list(
    values=NULL
))
