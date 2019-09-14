# vi: fdm=marker ts=4 et cc=80 tw=80

# NcbiGeneConn {{{1
################################################################################

# Declaration {{{2
################################################################################

#' NCBI Gene connector class.
#'
#' This is the connector class for a NCBI Gene database.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('ncbi.gene')
#'
#' # Get an entry
#' e <- conn$getEntry('2833')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include NcbiEntrezConn.R
#' @export NcbiGeneConn
#' @exportClass NcbiGeneConn
NcbiGeneConn <- methods::setRefClass("NcbiGeneConn",
    contains='NcbiEntrezConn',

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(...) {

    callSuper(entrez.name='gene', entrez.tag='Entrezgene',
              entrez.id.tag='Gene-track_geneid', ...)
},

# Get entry page url {{{3
################################################################################

getEntryPageUrl=function(id) {
    # Overrides super class' method.

    fct <- function(x) {
        u <- c(.self$getPropValSlot('urls', 'base.url'), .self$.entrez.name)
        BiodbUrl(url=u, params=list(term=x))$toString()
    }
    return(vapply(id, fct, FUN.VALUE=''))
},


# Search by name {{{3
################################################################################

searchByName=function(name, max.results=NA_integer_) {
    # Overrides super class' method.

    ids <- NULL

    # Search by name
    if ( ! is.null(name))
        term <- paste0('"', name, '"', '[Gene Name]')

    # Set retmax
    if (is.na(max.results)) {
        xml <- .self$wsEsearch(term=term, retmax=0, retfmt='parsed')
        xpath <- "/eSearchResult/Count"
        retmax <- as.integer(XML::xpathSApply(xml, xpath, XML::xmlValue))
        if (length(retmax) == 0)
            retmax <- NA_integer_
    }
    else
        retmax <- max.results

    # Send request
    ids <- .self$wsEsearch(term=term, retmax=retmax, retfmt='ids')

    return(ids)
}

))
