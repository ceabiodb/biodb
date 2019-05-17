# vi: fdm=marker ts=4 et cc=80 

# Class declaration {{{1
################################################################

#' A class for representing a rectangle.
#'
#' This abstract class represents a rectangle, used for graphical
#' representation.
#'
#' @slot label A text label to associate with the shape.
#' @slot color A color, as a character string.
#'
#' @seealso \code{\link{BiodbShape}}, \code{\link{BiodbCircle}}.
#'
#' @exportClass BiodbRect
setClass('BiodbRect', contains = 'BiodbShape',
         slots = c(left = 'integer', bottom = 'integer',
                   right = 'integer', top = 'integer'))
