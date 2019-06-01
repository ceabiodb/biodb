# vi: fdm=marker ts=4 et cc=80 

# BiodbDbInfo {{{1
################################################################################

#' A class for describing the characteristics of a database.
#'
#' This class is used by \code{\link{BiodbDbsInfo}} for storing database
#' characteristics, and returning them through the \code{get()} method.
#' This class inherits from \code{\link{BiodbConnBase}}.
#'
#' @seealso \code{\link{BiodbDbsInfo}}, \code{\link{BiodbConnBase}}.
#'
#' @import methods
#' @include BiodbConnBase.R
#' @export BiodbDbInfo
#' @exportClass BiodbDbInfo
BiodbDbInfo <- methods::setRefClass("BiodbDbInfo",
                                    contains =  "BiodbConnBase")
