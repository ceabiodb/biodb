# vi: fdm=marker ts=4 et cc=80

# Class declaration {{{1
################################################################################

#' @include NcbiEntrezConn.R
NcbiPubchemConn <- methods::setRefClass("NcbiPubchemConn", contains='NcbiEntrezConn', fields=list(.db.name='character', .id.xmltag='character', .entry.xmltag='character', .id.urlfield='character'))

# Initialize {{{1
################################################################################

NcbiPubchemConn$methods( initialize=function(db.name, id.xmltag, entry.xmltag, id.urlfield, ...) {

    # Call parent constructor
    callSuper(...)
    .self$.abstract.class('NcbiPubchemConn')

    .self$.db.name <- db.name
    .self$.id.xmltag <- id.xmltag
    .self$.entry.xmltag <- entry.xmltag
    .self$.id.urlfield <- id.urlfield
})

# Do get entry content request {{{1
################################################################################

NcbiPubchemConn$methods( .doGetEntryContentRequest=function(id, concatenate=TRUE) {

    if (concatenate)
        url <- BiodbUrl(url=c(.self$getPropValSlot('urls', 'ws2.url'), .self$.db.name, .self$.id.urlfield, paste(id, collapse=','), 'XML'))$toString()
    else
        url <- vapply(id, function(x) BiodbUrl(url=c(.self$getPropValSlot('urls', 'ws2.url'), .self$.db.name, .self$.id.urlfield, x, 'XML'))$toString(), FUN.VALUE='')

    return(url)
})

# Get entry page url {{{1
################################################################################

NcbiPubchemConn$methods( getEntryPageUrl=function(id) {
    return(vapply(id, function(x) BiodbUrl(url=c(.self$getPropValSlot('urls', 'base.url'), .self$.db.name, x))$toString(), FUN.VALUE=''))
})

# Get entry image url {{{1
################################################################################

NcbiPubchemConn$methods( getEntryImageUrl=function(id) {

    urls <- rep(NA_character_, length(id))

    # Loop on all IDs
    i <- 0
    for(x in id) {

        i <- i + 1

        # Set params
        params <- list()
        params[[.self$.id.urlfield]] <- x
        params$t <- 'l'

        # Build URL
        urls[[i]] <- BiodbUrl(url=c(.self$getPropValSlot('urls', 'base.url'), 'image', 'imgsrv.fcgi'), params=params)$toString()
    }

    return(urls)
})

# Get entry content from database {{{1
################################################################################

NcbiPubchemConn$methods( getEntryContentFromDb=function(entry.id) {

    # Debug
    .self$message('info', paste0("Get entry content(s) for ", length(entry.id)," id(s)..."))

    URL.MAX.LENGTH <- 2048
    concatenate <- TRUE
    done <- FALSE

    while ( ! done) {

        done <- TRUE

        # Initialize return values
        content <- rep(NA_character_, length(entry.id))

        # Get URL requests
        url.requests <- .self$getEntryContentRequest(entry.id, concatenate=concatenate, max.length=URL.MAX.LENGTH)

        # Loop on all URLs
        for (url in url.requests) {

            # Send request
            xmlstr <- .self$getBiodb()$getRequestScheduler()$getUrl(url)

            if (is.na(xmlstr) || length(grep('PUGREST.BadRequest|PUGREST.NotFound', xmlstr)) > 0) {
                if (concatenate) {
                    .self$message('caution', "One of the IDs to retrieve is wrong.")
                    concatenate <- FALSE
                    done <- FALSE
                    break
                }
                next
            }

            # Parse XML
            xml <-  XML::xmlInternalTreeParse(xmlstr, asText=TRUE)

            # Get returned IDs
            ns <- c(pcns="http://www.ncbi.nlm.nih.gov")
            returned.ids <- XML::xpathSApply(xml, paste0("//pcns:", .self$.id.xmltag), XML::xmlValue, namespaces=ns)

            # Store contents
            content[match(returned.ids, entry.id)] <- vapply(XML::getNodeSet(xml, paste0("//pcns:", .self$.entry.xmltag), namespaces=ns), XML::saveXML, FUN.VALUE='')
        }
    }

    return(content)
})
