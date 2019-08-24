# vi: fdm=marker ts=4 et cc=80

# BiodbCircle {{{1
################################################################################

#' A class for representing a circle.
#'
#' This class represents a rectangle, used for graphical
#' representation. It is used by
#' KeggPathwayConn::extractPathwayMapShapes() method.
#'
#' Arguments to the constructor are:
#'
#' x: X coordinate of center.
#'
#' y: Y coordinate of center.
#'
#' r: Radius.
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
#' \dontrun{
#' c1$draw()
#' }
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
    .self$.x <- as.integer(x)
    .self$.y <- as.integer(y)
    .self$.r <- as.integer(r)
},

# Get X {{{3
################################################################################

getX=function() {
    ":\n\nGet the X coordinate.
    \nReturned value: The X coordinate.
    "

    return(.self$.x)
},

# Get Y {{{3
################################################################################

getY=function() {
    ":\n\nGet the Y coordinate.
    \nReturned value: The Y coordinate.
    "

    return(.self$.y)
},

# Get radius {{{3
################################################################################

getRadius=function() {
    ":\n\nGet the radius.
    \nReturned value: The radius.
    "

    return(.self$.r)
},

# Draw {{{3
################################################################################

draw=function() {
    ":\n\nDraw the circle on the current image.
    \nReturned value: none.
    "

    symbols(x=.self$.x, y=.self$.y,
            circles=.self$.r,
            bg=.self$getRgbColor(alpha=127),
            add=TRUE, inches=FALSE)
}

))
