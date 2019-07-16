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
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#' 
#' # Get a BiodbDbInfo object for a database:
#' mybiodb$getDbsInfo()$get('chebi')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#' mybiodb <- NULL
#'
#' @import methods
#' @include BiodbConnBase.R
#' @export BiodbDbInfo
#' @exportClass BiodbDbInfo
BiodbDbInfo <- methods::setRefClass("BiodbDbInfo",
                                    contains="BiodbConnBase")
