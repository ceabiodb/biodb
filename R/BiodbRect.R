# vi: fdm=marker ts=4 et cc=80 

# Class declaration {{{1
################################################################################

# Declaration {{{2
################################################################################

#' A class for representing a rectangle.
#'
#' This class represents a rectangle, used for graphical
#' representation.
#'
#' Arguments to the constructor are:
#'
#' left: Coordinate of left border.
#'
#' right: Coordinate of right border.
#'
#' top: Coordinate of top border.
#'
#' bottom: Coordinate of bottom border.
#'
#' @seealso \code{\link{BiodbShape}}, \code{\link{BiodbCircle}}.
#'
#' @include BiodbShape.R
#' @export BiodbRect
#' @exportClass BiodbRect
#'
#' @examples
#' # Create a rectangle instance
#' r <- BiodbRect(left=10, top=10, bottom=20, right=30, color='yellow')
#'
#' # Draw a rectangle on current image
#' \dontrun{
#' r$draw()
#' }
#'
BiodbRect <- methods::setRefClass('BiodbRect',
    contains='BiodbShape',
    fields=list(.left='integer',
                .bottom='integer',
                .right='integer',
                .top='integer'),

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(left, top, bottom, right, ...) {
    callSuper(...)
    .self$.left     <- as.integer(left)
    .self$.right    <- as.integer(right)
    .self$.top      <- as.integer(top)
    .self$.bottom   <- as.integer(bottom)
},

# Draw {{{3
################################################################################

draw=function() {
    # Overrides super class' method.

    rect(.self$.left, .self$.bottom, .self$.right, .self$.top,
         col=.self$getRgbColor(alpha=127), border=NA)
}

))
