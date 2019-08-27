# vi: fdm=marker ts=4 et cc=80 tw=80

# KeggReactionConn {{{1
################################################################################

#' The connector class to KEGG Reaction database.
#'
#' This is a concrete connector class. It must never be instantiated directly,
#' but instead be instantiated through the factory \code{\link{BiodbFactory}}.
#' Only specific methods are described here. See super classes for the
#' description of inherited methods.
#'
#' @seealso \code{\link{BiodbFactory}}, \code{\link{KeggConn}}.
#'
#' @include KeggConn.R
#' @export KeggReactionConn
#' @exportClass KeggReactionConn
KeggReactionConn <- methods::setRefClass("KeggReactionConn",
    contains="KeggConn",

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(...) {
    callSuper(db.name='reaction', db.abbrev='rn', ...)
},


))
