# vi: fdm=marker ts=4 et cc=80 

# BiodbShape {{{1
################################################################################

# Declaration {{{2
################################################################################

#' A class for representing a shape.
#'
#' This abstract class represents a shape, used for graphical representation.
#'
#' Arguments to the constructor are:
#'
#' label: A text label to associate with the shape.
#'
#' color: A color, as a character string.
#'
#' @seealso \code{\link{BiodbRect}}, \code{\link{BiodbCircle}}.
#'
#' @examples
#' # Create a circle instance
#' c <- BiodbCircle(x=12, y=5, r=3, label='MyCircle')
#'
#' # Create a rectangle instance
#' r <- BiodbRect(left=10, top=10, bottom=20, right=30, color='yellow')
#'
#' @export BiodbShape
#' @exportClass BiodbShape
BiodbShape <- methods::setRefClass('BiodbShape',
    fields=list(.label='character',
                .color='character'),

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(label=NA_character_,
                    color=NA_character_) {
    .self$.label <- label
    .self$.color <- color
},

# Get label {{3
################################################################################

getLabel=function() {
    ":\n\nGets the label associated with this shape.
    \nReturned value: The label.
    "

    return(.self$.label)
},

# Get color {{3
################################################################################

getColor=function() {
    ":\n\nGets the color associated with this shape.
    \nReturned value: The color name as a string.
    "

    return(.self$.color)
},

# Get RGB color {{3
################################################################################

getRgbColor=function(alpha=255) {
    ":\n\nGets the RGB color associated with this shape.
    \nalpha: The value to use for the alpha channel when building the RGB color
    object.
    \nReturned value: The color as an RGB color object.
    "

    c <- col2rgb(.self$.color)
    c <- rgb(c[1,], c[2,], c[3,], alpha, maxColorValue=255)

    return(c)
},

# Draw {{3
################################################################################

draw=function() {
    ":\n\nDraw the shape on the current image.
    \nReturned value: None.
    "

    invisible()
}

))
