#' This class represents an SQL field.
#'
#' @import R6
#' @include BiodbSqlExpr.R
BiodbSqlField <- R6::R6Class("BiodbSqlField",
inherit=BiodbSqlExpr,

public=list(

#' @description
#' Constructor.
#' @param table The table name.
#' @param field The field name.
#' @return A new instance.
initialize=function(table=NA_character_, field) {
    private$table <- table
    private$field <- field
},

#' @description
#' Converts into a string.
#' @return A string containing the SQL expression.
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
