#' This class represents an SQL value.
#'
#' @include BiodbSqlExpr.R
BiodbSqlValue <- methods::setRefClass("BiodbSqlValue",
    contains="BiodbSqlExpr",
    fields=list(
        .value='ANY'
        ),

methods=list(

initialize=function(value) {
    .self$.value <- value
},

toString=function() {

    # Quote strings
    if (is.character(.self$.value))
        s <- paste0('"', .self$.value, '"')
    else
        s <- as.character(.self$.value)

    return(s)
}

))
