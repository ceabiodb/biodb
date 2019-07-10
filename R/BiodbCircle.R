# vi: fdm=marker ts=4 et cc=80 

# BiodbCircle {{{1
################################################################################

#' A class for representing a circle.
#'
#' This class represents a rectangle, used for graphical
#' representation. It is used inside by
#' KeggPathwayConn::extractPathwayMapShapes() method.
#'
#' @field x      X coordinate of center. 
#' @field y      Y coordinate of center. 
#' @field r      Radius.
#'
#' @seealso \code{\link{BiodbShape}}, \code{\link{BiodbRect}}.
#'
#' @examples
#' # Create an instance
#' c1 <- BiodbCircle(x=12, y=5, r=3)
#'
#' # Since it inherits from BiodbShape, a color and a label can be set
#' c2 <- BiodbCircle(x=12, y=5, r=3, color='blue', label='Circle 2')
#'
#' # Getting center
#' c1$getX()
#' c1$getY()
#'
#' # Getting radius
#' c1#getRadius()
#'
#' # Draw a circle on the current image
#' c1$draw()
#'
#' @include BiodbShape.R
#' @export BiodbCircle
#' @exportClass BiodbCircle
BiodbCircle <- methods::setRefClass('BiodbCircle',
    contains='BiodbShape',
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

# Get X {{{3
################################################################################

getX=function() {
    'Return the X coordinate.'
    
    return(.self$.x)
},

# Get Y {{{3
################################################################################

getY=function() {
    'Return the Y coordinate.'
    
    return(.self$.y)
},

# Get radius {{{3
################################################################################

getRadius=function() {
    'Return the radius.'
    
    return(.self$.r)
},

# Draw {{{3
################################################################################

draw=function() {
    'Draw the shape on the current image.'
    
    symbols(x=.self$.x, y=.self$.y,
            circles=.self$.r,
            bg=.self$getRgbColor(alpha=127),
            add=TRUE, inches=FALSE)
}

))
