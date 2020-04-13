#' This abstract class represents an SQL expression.
BiodbSqlExpr <- methods::setRefClass("BiodbSqlExpr",

methods=list(

toString=function() {
    stop("This method is abstract.")
}

))

