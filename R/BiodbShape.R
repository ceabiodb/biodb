# vi: fdm=marker ts=4 et cc=80 

# BiodbShape {{{1
################################################################################

#' A class for representing a shape.
#'
#' This abstract class represents a shape, used for graphical representation.
#'
#' @field label A text label to associate with the shape.
#' @field color A color, as a character string.
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
    'Return the label associated with this shape.'
    
    return(.self$.label)
},

# Get color {{3
################################################################################

getColor=function() {
    'Return the color associated with this shape.'
    
    return(.self$.color)
},

# Get RGB color {{3
################################################################################

getRgbColor=function(alpha=255) {
    'Return the RGB color associated with this shape.'
    
    c <- col2rgb(.self$.color)
    c <- rgb(c[1,], c[2,], c[3,], alpha, maxColorValue=255)
    
    return(c)
},

# Draw {{3
################################################################################

draw=function() {
    'Draw the shape on the current image.'
}

))
