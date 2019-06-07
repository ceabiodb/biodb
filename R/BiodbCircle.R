# vi: fdm=marker ts=4 et cc=80 

# BiodbCircle {{{1
################################################################################

#' A class for representing a circle.
#'
#' This class represents a rectangle, used for graphical
#' representation.
#'
#' @field x      X coordinate of center. 
#' @field y      Y coordinate of center. 
#' @field r      Radius.
#'
#' @seealso \code{\link{BiodbShape}}, \code{\link{BiodbRect}}.
#'
#' @include BiodbShape.R
#' @export BiodbCircle
#' @exportClass BiodbCircle
BiodbCircle <- methods::setRefClass('BiodbCircle',
                                    contains='BiodbShape',

# Fields {{{2
################################################################################

fields=list(.x='integer',
            .y='integer',
            .r='integer'),

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(x, y, r, ...) {
    callSuper(...)
    .self$.x <- x
    .self$.y <- y
    .self$.r <- r
},

# Get X {{3
################################################################################

getX=function() {
    'Return the X coordinate.'
    
    return(.self$.x)
},

# Get Y {{3
################################################################################

getY=function() {
    'Return the Y coordinate.'
    
    return(.self$.y)
},

# Get radius {{3
################################################################################

getRadius=function() {
    'Return the radius.'
    
    return(.self$.r)
},

# Draw {{3
################################################################################

draw=function() {
    'Draw the shape on the current image.'
    
    symbols(x=.self$.x, y=.self$.y,
            circles=.self$.r,
            bg=.self$getRgbColor(alpha=127),
            add=TRUE, inches=FALSE)
}

))
