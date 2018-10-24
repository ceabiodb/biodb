# vi: fdm=marker

# Class declaration {{{1
################################################################

#' A class for describing the characteristics of a database.
#'
#' This class is used by \code{\link{BiodbDbsInfo}} for storing database characteristics, and returning them through the \code{get()} method. This class inherits from \code{\link{BiodbConnBase}}.
#'
#' @seealso \code{\link{BiodbDbsInfo}}, \code{\link{BiodbConnBase}}.
#'
#' @import methods
#' @include ChildObject.R
#' @export BiodbDbInfo
#' @exportClass BiodbDbInfo
BiodbDbInfo <- methods::setRefClass("BiodbDbInfo", contains =  "BiodbConnBase")

# Constructor {{{1
################################################################

BiodbDbInfo$methods( initialize = function(...) {

	callSuper(...)
})
