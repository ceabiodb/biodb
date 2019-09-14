# vi: fdm=marker ts=4 et cc=80 tw=80

# NcbiPubchemConn {{{1
################################################################################

# Declaration {{{2
################################################################################

#' NCBI PubChem connector abstractclass.
#'
#' This is an abstract class, mother class of all NCBI PubChem connector
#' classes.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('ncbi.pubchem.comp')
#'
#' # Get an entry
#' e <- conn$getEntry('2')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include NcbiEntrezConn.R
#' @export NcbiPubchemConn
#' @exportClass NcbiPubchemConn
NcbiPubchemConn <- methods::setRefClass("NcbiPubchemConn",
    contains='NcbiEntrezConn',
    fields=list(
        .db.name='character',
        .id.xmltag='character',
        .entry.xmltag='character',
        .id.urlfield='character'),

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(db.name, id.xmltag, entry.xmltag, id.urlfield, ...) {

    # Call parent constructor
    callSuper(...)
    .self$.abstractClass('NcbiPubchemConn')

    .self$.db.name <- db.name
    .self$.id.xmltag <- id.xmltag
    .self$.entry.xmltag <- entry.xmltag
    .self$.id.urlfield <- id.urlfield
},

# Get entry page url {{{3
################################################################################

getEntryPageUrl=function(id) {
    # Overrides super class' method.

    fct <- function(x) {
        u <- c(.self$getPropValSlot('urls', 'base.url'), .self$.db.name, x)
        BiodbUrl(url=u)$toString()
    }

    return(vapply(id, fct, FUN.VALUE=''))
},

# Get entry image url {{{3
################################################################################

getEntryImageUrl=function(id) {
    # Overrides super class' method.

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
        u <- c(.self$getPropValSlot('urls', 'base.url'), 'image', 'imgsrv.fcgi')
        urls[[i]] <- BiodbUrl(url=u, params=params)$toString()
    }

    return(urls)
},

# Get entry content from database {{{3
################################################################################

getEntryContentFromDb=function(entry.id) {
    # Overrides super class' method.

    # Debug
    .self$info("Get entry content(s) for ", length(entry.id)," id(s)...")

    URL.MAX.LENGTH <- 2048
    concatenate <- TRUE
    done <- FALSE

    while ( ! done) {

        done <- TRUE

        # Initialize return values
        content <- rep(NA_character_, length(entry.id))

        # Get URL requests
        url.requests <- .self$getEntryContentRequest(entry.id,
                                                     concatenate=concatenate,
                                                     max.length=URL.MAX.LENGTH)

        # Loop on all URLs
        for (url in url.requests) {

            # Send request
            xmlstr <- .self$getBiodb()$getRequestScheduler()$getUrl(url)

            re <- 'PUGREST.BadRequest|PUGREST.NotFound'
            if (is.na(xmlstr) || length(grep(re, xmlstr)) > 0) {
                if (concatenate) {
                    .self$caution("One of the IDs to retrieve is wrong.")
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
            xpath <- paste0("//pcns:", .self$.id.xmltag)
            returned.ids <- XML::xpathSApply(xml, xpath, XML::xmlValue,
                                             namespaces=ns)

            # Store contents
            xpath <- paste0("//pcns:", .self$.entry.xmltag)
            nodes <- XML::getNodeSet(xml, xpath, namespaces=ns)
            c <- vapply(nodes, XML::saveXML, FUN.VALUE='')
            content[match(returned.ids, entry.id)] <- c
        }
    }

    return(content)
},

# Private methods {{{2
################################################################################

# Do get entry content request {{{3
################################################################################

.doGetEntryContentRequest=function(id, concatenate=TRUE) {

    if (concatenate) {
        u <- c(.self$getPropValSlot('urls', 'ws2.url'), .self$.db.name,
               .self$.id.urlfield, paste(id, collapse=','), 'XML')
        url <- BiodbUrl(url=u)$toString()
    }
    else {
        fct <- function(x) {
            u <- c(.self$getPropValSlot('urls', 'ws2.url'), .self$.db.name,
                   .self$.id.urlfield, x, 'XML')
            BiodbUrl(url=u)$toString()
        }
        url <- vapply(id, fct, FUN.VALUE='')
    }

    return(url)
}

))
