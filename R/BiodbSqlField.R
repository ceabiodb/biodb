#' This class represents an SQL field.
#'
#' @import R6
#' @include BiodbSqlExpr.R
BiodbSqlField <- R6::R6Class("BiodbSqlField",
inherit=BiodbSqlExpr,

public=list(

#' @description
#' Initializer.
#' @param table The table name.
#' @param field The field name.
#' @return Nothing.
initialize=function(table=NA_character_, field) {
    private$table <- table
    private$field <- field

    return(invisible(NULL))
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
