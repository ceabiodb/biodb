# vi: fdm=marker ts=4 et cc=80 tw=80

# NcbiCcdsConn {{{1
################################################################################

# Declaration {{{2
################################################################################

#' NCBI CCDS connector class.
#'
#' This is the connector class for a NCBI CCDS database.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('ncbi.ccds')
#'
#' # Get an entry
#' e <- conn$getEntry('CCDS12227.1')
#'
#' # Terminate instance.
#' mybiodb$terminate()
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
