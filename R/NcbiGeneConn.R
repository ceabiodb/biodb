# vi: fdm=marker ts=4 et cc=80

# Class declaration {{{1
################################################################################

#' @include NcbiEntrezConn.R
#' @include BiodbSearchable.R
NcbiGeneConn <- methods::setRefClass("NcbiGeneConn", contains=c('NcbiEntrezConn', 'BiodbSearchable'))

# Initialize {{{1
################################################################################

NcbiGeneConn$methods( initialize=function(...) {

    callSuper(entrez.name='gene', entrez.tag='Entrezgene', entrez.id.tag='Gene-track_geneid', ...)
})

# Get entry page url {{{1
################################################################################

NcbiGeneConn$methods( getEntryPageUrl=function(id) {
    return(vapply(id, function(x) BiodbUrl(url=c(.self$getPropValSlot('urls', 'base.url'), .self$.entrez.name), params=list(term=x))$toString(), FUN.VALUE=''))
})

# Get entry image url {{{1
################################################################################

NcbiGeneConn$methods( getEntryImageUrl=function(id) {
    return(rep(NA_character_, length(id)))
})

# Search by name {{{1
################################################################################

NcbiGeneConn$methods( searchByName=function(name, max.results=NA_integer_) {

    ids <- NULL

    # Search by name
    if ( ! is.null(name))
        term <- paste0('"', name, '"', '[Gene Name]')

    # Set retmax
    if (is.na(max.results)) {
        xml <- .self$ws.esearch(term=term, retmax=0, retfmt='parsed')
        retmax <- as.integer(XML::xpathSApply(xml, "/eSearchResult/Count", XML::xmlValue))
        if (length(retmax) == 0)
            retmax <- NA_integer_
    }
    else
        retmax <- max.results

    # Send request
    ids <- .self$ws.esearch(term=term, retmax=retmax, retfmt='ids')

    return(ids)
})

