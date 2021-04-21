#' This class represents an SQL field.
#'
#' @include BiodbSqlExpr.R
BiodbSqlField <- R6::R6Class("BiodbSqlField",
inherit=BiodbSqlExpr,

public=list(

initialize=function(table=NA_character_, field) {
    private$table <- table
    private$field <- field
},

toString=function() {
    s <- DBI::dbQuoteIdentifier(DBI::ANSI(), private$field)
    if ( ! is.na(private$table))
        s <- paste(DBI::dbQuoteIdentifier(DBI::ANSI(), private$table), s,
                   sep='.')
    return(s)
}
),

private=list(
    table=NULL,
    field=NULL
))
