# vi: fdm=marker ts=4 et cc=80 

# Class declaration {{{1
################################################################

#' A class for representing a shape.
#'
#' This abstract class represents a shape, used for graphical representation.
#'
#' @slot label A text label to associate with the shape.
#' @slot color A color, as a character string.
#'
#' @seealso \code{\link{BiodbRect}}, \code{\link{BiodbCircle}}.
#'
#' @exportClass BiodbShape
setClass('BiodbShape', slots = c(label = 'character', color = 'character'))
