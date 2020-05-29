#' This class represents an SQL list.
#'
#' @include BiodbSqlExpr.R
BiodbSqlList <- methods::setRefClass("BiodbSqlList",
    contains="BiodbSqlExpr",
    fields=list(
        .values='ANY'),

methods=list(

initialize=function(values) {
    .self$.values <- values
},

toString=function() {

    # Quote strings
    if (is.character(.self$.values)) {
        fct <- function(v) paste0('"', v, '"')
        s <- vapply(.self$.values, fct, FUN.VALUE='')
    }

    # Collapse and convert to string
    s <- paste0('(', paste(s, collapse=', '), ')')

    return(s)
}

))
