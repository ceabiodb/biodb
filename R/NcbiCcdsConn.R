# vi: fdm=marker ts=4 et cc=80 tw=80

# NcbiCcdsConn {{{1
################################################################################

#' NCBI CCDS connector class.
#'
#' @include NcbiConn.R
#' @export NcbiCcdsConn
#' @exportClass NcbiCcdsConn
NcbiCcdsConn <- methods::setRefClass("NcbiCcdsConn",
    contains="NcbiConn",

# Public methods {{{2
################################################################################

methods=list(

# Get entry page url {{{3
################################################################################

getEntryPageUrl=function(id) {
    # Overrides super class' method.

    fct <- function(x) {
        u <- c(.self$getPropValSlot('urls', 'base.url'), 'CcdsBrowse.cgi')
        p <- list(REQUEST='CCDS', GO='MainBrowse', DATA=x)
        BiodbUrl(url=u, params=p)$toString()
    }
    return(vapply(id, fct, FUN.VALUE=''))
},


# Private methods {{{2
################################################################################

# Do get entry content request {{{3
################################################################################

.doGetEntryContentRequest=function(id, concatenate=TRUE) {
    return(.self$getEntryPageUrl(id))
},

# Get entry ids {{{3
################################################################################

.doGetEntryIds=function(max.results=NA_integer_) {
    return(NULL)
}

))
