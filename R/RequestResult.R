#' Class RequestResult.
#'
#' Represents the result of a request.
#'
#' @import  R6
RequestResult <- R6::R6Class("RequestResult",

public=list(
),

private=list(
    content=NULL,
    retry=NULL,
    errMsg=NULL,
    status=NULL
))
