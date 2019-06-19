# vi: fdm=marker ts=4 et cc=80 tw=80

# Class declaration {{{1
################################################################################

#' The connector class to KEGG Pathway database.
#'
#' This is a concrete connector class. It must never be instantiated directly, but instead be instantiated through the factory \code{\link{BiodbFactory}}. Only specific methods are described here. See super classes for the description of inherited methods.
#'
#' @seealso \code{\link{BiodbFactory}}, \code{\link{KeggConn}}.
#'
#' @include KeggConn.R
#' @export KeggGenesConn
#' @exportClass KeggGenesConn
KeggGenesConn <- methods::setRefClass("KeggGenesConn", contains=c("KeggConn"))

# Initialize {{{1
################################################################################

KeggGenesConn$methods( initialize=function(...) {
    callSuper(db.name='genes', ...)
})

# Get entry image url {{{1
################################################################################

KeggGenesConn$methods( getEntryImageUrl=function(id) {
    return(rep(NA_character_, length(id)))
})

