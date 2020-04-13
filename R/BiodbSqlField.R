#' This class represents an SQL field.
#'
#' @include BiodbSqlExpr.R
BiodbSqlField <- methods::setRefClass("BiodbSqlField",
    contains="BiodbSqlExpr",
    fields=list(
        .table='character',
        .field='character'),

methods=list(

initialize=function(table=NA_character_, field) {
    .self$.table <- table
    .self$.field <- field
},

toString=function() {
    s <- DBI::dbQuoteIdentifier(DBI::ANSI(), .self$.field)
    if ( ! is.na(.self$.table))
        s <- paste(DBI::dbQuoteIdentifier(DBI::ANSI(), .self$.table), s,
                   sep='.')
    return(s)
}

))
