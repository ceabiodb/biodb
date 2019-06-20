# vi: fdm=marker ts=4 et cc=80 tw=80

# BiodbSearchable {{{1
################################################################################

#' An abstract class (more like an interface) to model a database that can be
#' searchable.
#'
#' The class implementing this interface, must be searchable in a generic way.
#' For instance, its entries must be searchable by name.
#'
#' @seealso \code{\link{BiodbConn}}.
#'
#' @import methods
#' @include BiodbObject.R
#' @export BiodbSearchable
#' @exportClass BiodbSearchable
BiodbSearchable <- methods::setRefClass("BiodbSearchable",
    contains='BiodbObject',
    fields=list(
        .ext='character'
    ),

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(...) {

    callSuper(...)
    .self$.abstractClass('BiodbSearchable')
},

# Search by name {{{3
################################################################################

searchByName=function(name, max.results=NA_integer_) {

    .self$.abstractMethod()
}

))
