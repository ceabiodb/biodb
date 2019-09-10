# vi: fdm=marker ts=4 et cc=80 tw=80

# KeggOrthologyConn {{{1
################################################################################

#' The connector class to KEGG Orthology database.
#'
#' This is a concrete connector class. It must never be instantiated directly,
#' but instead be instantiated through the factory \code{\link{BiodbFactory}}.
#' Only specific methods are described here. See super classes for the
#' description of inherited methods.
#'
#' @seealso \code{\link{BiodbFactory}}, \code{\link{KeggConn}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('kegg.orthology')
#'
#' # Get an entry
#' e <- conn$getEntry('K12668')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include KeggConn.R
#' @export KeggOrthologyConn
#' @exportClass KeggOrthologyConn
KeggOrthologyConn <- methods::setRefClass("KeggOrthologyConn",
    contains=c("KeggConn"),

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(...) {
    callSuper(db.name='orthology', db.abbrev='ko', ...)
}

))
