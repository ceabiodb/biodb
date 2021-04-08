#' Range class.
#'
#' A class for storing min/max range or value/tolerance.
#'
#' @examples
#' # Convert a min/max range into a value/ppm tolerance:
#' rng <- Range$new(min=0.4, max=0.401)
#' value <- rng$getValue()
#' ppm <- rng$getPpm()
#' 
#' @import R6
#' @import chk 
#' @export
Range <- R6::R6Class('Range',
public=list(
            
#' @description
#' Constructor.
#' @param min The minimum value of the range.
#' @param max The maximum value of the range.
#' @param value The value.
#' @param delta The delta tolerance.
#' @param ppm The PPM tolerance.
#' @param tol The tolerance value, whose type (ppm or delta) is specified by
#'        the "tolType" parameter.
#' @param tolType The type of the tolerance value specified by the "tol"
#'        parameter.
#' @return A new instance.
#' @examples
#' # Create an instance from min and max:
#' Range$new(min=1.2, max=1.5)
initialize=function(min=NULL, max=NULL, value=NULL, delta=NULL,
                    ppm=NULL, tol=NULL, tolType=c('delta', 'plain', 'ppm')) {
    chk::chk_null_or(min, chk::chk_number)
    chk::chk_null_or(max, chk::chk_number)
    chk::chk_null_or(value, chk::chk_number)
    chk::chk_null_or(delta, chk::chk_number)
    chk::chk_null_or(ppm, chk::chk_number)
    chk::chk_null_or(delta, chk::chk_number)
    tolType <- match.arg(tolType)

    private$min <- min
    private$max <- max
    private$value <- value
    private$delta <- delta
    private$ppm <- ppm
    if ( ! is.null(tol)) {
        if (tolType == 'ppm')
            private$ppm <- tol
        else
            private$delta <- tol
    }
}

#' @description
#' Gets the middle value of the range.
#' @return The middle value.
,getValue=function() {
    
    if (is.null(private$value)) {
        chk::chk_number(private$min)
        chk::chk_number(private$max)
        private$value <- (private$min + private$max) / 2
    }

    return(private$value)
} 

#' @description
#' Gets the minimum value of the range.
#' @return The minimum value.
,getMin=function() {
    
    if (is.null(private$min)) {
        chk::chk_number(private$value)
        if (chk::vld_number(private$delta))
            private$min <- private$value - private$delta
        else {
            chk::chk_number(private$ppm)
            private$min <- private$value - private$value * private$ppm * 1e-6
        }
    }

    return(private$min)
}

#' @description
#' Gets the maximum value of the range.
#' @return The maximum value.
,getMax=function() {
    
    if (is.null(private$max)) {
        chk::chk_number(private$value)
        if (chk::vld_number(private$delta))
            private$max <- private$value + private$delta
        else {
            chk::chk_number(private$ppm)
            private$max <- private$value + private$value * private$ppm * 1e-6
        }
    }

    return(private$max)
}

#' @description
#' Get the min/max range.
#' @return A list containing two fields: "min" and "max.
,getMinMax=function() {
    return(list(min=self$getMin(), max=self$getMax()))
}

#' @description
#' Gets the delta tolerance of the range.
#' @return The delta tolerance.
,getDelta=function() {
    
    if (is.null(private$delta)) {
        if (chk::vld_number(private$min) && chk::vld_number(private$max))
            private$delta <- (private$max - private$min) / 2
        else {
            chk::chk_number(private$value)
            chk::chk_number(private$ppm)
            private$delta <- private$value * private$ppm * 1e-6
        }
    }

    return(private$delta)
}

#' @description
#' Gets the PPM tolerance of the range.
#' @return The tolerance in PPM.
,getPpm=function() {
    
    if (is.null(private$ppm)) {
        if (chk::vld_number(private$value) && chk::vld_number(private$delta))
            private$ppm <- private$delta * 1e6 / private$value
        else {
            chk::chk_number(private$min)
            chk::chk_number(private$max)
            private$ppm <- ((private$max - private$min) * 1e6
                            / (private$min + private$max))
        }
    }

    return(private$ppm)
}
)

,private=list(
    min=NULL,
    max=NULL,
    value=NULL,
    delta=NULL,
    ppm=NULL
))
