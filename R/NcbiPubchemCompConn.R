# vi: fdm=marker ts=4 et cc=80

# Class declaration {{{1
################################################################################

#' @include NcbiPubchemConn.R
#' @include BiodbSearchable.R
#' @include BiodbCompounddbConn.R
NcbiPubchemCompConn <- methods::setRefClass("NcbiPubchemCompConn", contains = c("NcbiPubchemConn", 'BiodbCompounddbConn', 'BiodbSearchable'))

# Initialize {{{1
################################################################################

NcbiPubchemCompConn$methods( initialize = function(...) {
    callSuper(db.name = 'compound', id.xmltag = 'PC-CompoundType_id_cid', entry.xmltag = 'PC-Compound', id.urlfield = 'cid', entrez.name = 'pccompound', ...)
})

# Search by name {{{1
################################################################################

NcbiPubchemCompConn$methods( searchByName = function(name, max.results = NA_integer_) {
    return(.self$searchCompound(name = name, max.results = max.results))
})

# Search compound {{{1
################################################################################

NcbiPubchemCompConn$methods( searchCompound = function(name = NULL, mass = NULL, mass.field = NULL, mass.tol = 0.01, mass.tol.unit = 'plain', max.results = NA_integer_) {
        
    .self$.checkMassField(mass = mass, mass.field = mass.field)

    term <- character()

    # Search by name
    if ( ! is.null(name))
        term <- paste0('"', name, '"', '[IUPACName]')

    # Search by mass
    if ( ! is.null(mass) && ! is.null(mass.field)) {

        mass.field <- .self$getBiodb()$getEntryFields()$getRealName(mass.field)

        if ( ! mass.field %in% c('monoisotopic.mass' ,'molecular.mass'))
            .self$message('caution', paste0('Mass field "', mass.field, '" is not handled.'))

        else {

            pubchem.mass.field <- if (mass.field == 'monoisotopic.mass') 'MonoisotopicMass' else 'MolecularWeight'

            if (mass.tol.unit == 'ppm') {
                mass.min <- mass * (1 - mass.tol * 1e-6)
                mass.max <- mass * (1 + mass.tol * 1e-6)
            } else {
                mass.min <- mass - mass.tol
                mass.max <- mass + mass.tol
            }

            mass.term <- paste0(mass.min, ':', mass.max, '[', pubchem.mass.field, ']')

            if (length(term) > 0)
                term <- paste(term, 'AND', mass.term)
            else
                term <- mass.term
        }
    }

    # Set retmax
    if (is.na(max.results)) {
        xml <- .self$ws.esearch(term = term, retmax = 0, retfmt = 'parsed')
        retmax <- as.integer(XML::xpathSApply(xml, "/eSearchResult/Count", XML::xmlValue))
        if (length(retmax) == 0)
            retmax = NA_integer_
    }
    else
        retmax <- max.results

    # Send request
    ids <- .self$ws.esearch(term = term, retmax = retmax, retfmt = 'ids')

    return(ids)
})

