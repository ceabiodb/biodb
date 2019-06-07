# vi: fdm=marker ts=4 et cc=80

# Class declaration {{{1
################################################################################

#' An abstract class (more like an interface) to model a database that can be searchable.
#'
#' The class implementing this interface, must be searchable in a generic way. For instance, its entries must be searchable by name.
#'
#' @seealso \code{\link{BiodbConn}}.
#'
#' @import methods
#' @include BiodbObject.R
#' @export BiodbSearchable
#' @exportClass BiodbSearchable
BiodbSearchable <- methods::setRefClass("BiodbSearchable", contains='BiodbObject', fields=list(.ext='character'))

# Initialize {{{1
################################################################0

BiodbSearchable$methods( initialize=function(...) {

    callSuper(...)
    .self$.abstractClass('BiodbSearchable')
})

# Search by name {{{1
################################################################0

BiodbSearchable$methods( searchByName=function(name, max.results=NA_integer_) {

    .self$.abstractMethod()
})
