# vi: fdm=marker ts=4 et cc=80 tw=80

# KeggCompoundConn {{{1
################################################################################

# Declaration {{{2
################################################################################

#' The connector class to KEGG Compound database.
#'
#' This is a concrete connector class. It must never be instantiated directly,
#' but instead be instantiated through the factory \code{\link{BiodbFactory}}.
#' Only specific methods are described here. See super classes for the
#' description of inherited methods.
#'
#' @seealso \code{\link{BiodbFactory}}, \code{\link{KeggConn}},
#' \code{\link{BiodbCompounddbConn}}, \code{\link{KeggPathwayConn}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Create a connector to KEGG Compound
#' conn <- mybiodb$getFactory()$createConn('kegg.compound')
#'
#' # Search for compounds by exact mass
#' conn$wsFindExactMass(mass=174.05, retfmt='parsed')
#'
#' # Search for compounds by molecular weight 
#' conn$wsFindMolecularWeight(mass=300, retfmt='parsed')
#'
#' # Get pathway IDs related to compounds
#' pathway.ids=conn$getPathwayIds(c('C02648', 'C06144'), org='mmu')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include KeggConn.R
#' @include BiodbCompounddbConn.R
#' @export KeggCompoundConn
#' @exportClass KeggCompoundConn
KeggCompoundConn <- methods::setRefClass("KeggCompoundConn",
    contains=c("KeggConn", "BiodbCompounddbConn"),

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(...) {
    callSuper(db.name='compound', db.abbrev='cpd', ...)
},

# Web service find exact mass {{{3
################################################################################

wsFindExactMass=function(mass=NA_real_, mass.min=NA_real_, mass.max=NA_real_,
                         retfmt=c('plain', 'request', 'parsed', 'ids')) {
    ":\n\nSearches for entries by mass.
    You must either provide a single mass through `mass` parameter or provide a
    range through `mass.min` and `mass.max`.
    See http://www.kegg.jp/kegg/docs/keggapi.html for details.
    \nmass: Single mass.
    \nmass.min: Minimal mass.
    \nmass.max: Maximal mass.
    \nretfmt: Set the format to use for the returned value. 'plain' will return the raw results from the server, as a character value. 'request' will return the request as it would have been sent, as a BiodbRequest object. 'parsed' will return a data frame. 'ids' will return a character vector containing the IDs of the matching entries.
    \nReturned value: Depending on `retfmt`.
    "

    retfmt <- match.arg(retfmt)

    # Build request
    u <- c(.self$getPropValSlot('urls', 'ws.url'), 'find', .self$.db.name)
    if ( ! is.na(mass))
        umass <- mass
    else if ( ! is.na(mass.min) && ! is.na(mass.max))
        umass <- paste(mass.min, mass.max, sep='-')
    else
        .self$error('You need to specify either mass parameter or both',
                    ' mass.min and mass.max.')
    u <- c(u, umass, 'exact_mass')
    url <- BiodbUrl(url=u)$toString()
    request <- BiodbRequest(method='get', url=BiodbUrl(url=url))
    if (retfmt == 'request')
        return(request)

    # Send request
    results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

    # Parse results
    if (retfmt != 'plain') {

        # Parse
        if (length(grep('^[[:space:]]*$', results, perl=TRUE)) == 0) {
            readtc <- textConnection(results, "r", local=TRUE)
            df <- read.table(readtc, sep="\t", quote='', stringsAsFactors=FALSE)
            close(readtc)
            results <- df
        } else {
            results <- data.frame()
        }

        # Get IDs
        if (retfmt == 'ids')
            results <- if (ncol(results) > 0) results[[1]] else character()
    }

    return(results)
},

# Web service find molecural weight {{{3
################################################################################

wsFindMolecularWeight=function(mass=NA_real_, mass.min=NA_real_,
                               mass.max=NA_real_,
                               retfmt=c('plain', 'request', 'parsed', 'ids')) {
    ":\n\nSearches for entries by molecular mass.
    You must either provide a single mass through `mass` parameter or provide a
    range through `mass.min` and `mass.max`.
    See http://www.kegg.jp/kegg/docs/keggapi.html for details.
    \nmass: Single mass.
    \nmass.min: Minimal mass.
    \nmass.max: Maximal mass.
    \nretfmt: Set the format to use for the returned value. 'plain' will return the raw results from the server, as a character value. 'request' will return the request as it would have been sent, as a BiodbRequest object. 'parsed' will return a data frame. 'ids' will return a character vector containing the IDs of the matching entries.
    \nReturned value: Depending on `retfmt`.
    "

    retfmt <- match.arg(retfmt)

    # Build request
    u <- c(.self$getPropValSlot('urls', 'ws.url'), 'find', .self$.db.name)
    if ( ! is.na(mass))
        umass <- mass
    else if ( ! is.na(mass.min) && ! is.na(mass.max))
        umass <- paste(mass.min, mass.max, sep='-')
    else
        .self$error('You need to specify either mass parameter or both',
                    ' mass.min and mass.max.')
    u <- c(u, umass, 'mol_weight')
    url <- BiodbUrl(url=u)$toString()
    request <- BiodbRequest(method='get', url=BiodbUrl(url=url))
    if (retfmt == 'request')
        return(request)

    # Send request
    results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

    # Parse results
    if (retfmt != 'plain') {

        # Parse
        if (length(grep('^[[:space:]]*$', results, perl=TRUE)) == 0) {
            readtc <- textConnection(results, "r", local=TRUE)
            df <- read.table(readtc, sep="\t", quote='', stringsAsFactors=FALSE)
            close(readtc)
            results <- df
        } else {
            results <- data.frame()
        }

        # Get IDs
        if (retfmt == 'ids')
            results <- if (ncol(results) > 0) results[[1]] else character()
    }

    return(results)
},

# Search compound {{{3
################################################################################

searchCompound=function(name=NULL, mass=NULL, mass.field=NULL, mass.tol=0.01,
                        mass.tol.unit='plain', max.results=NA_integer_) {
    # Overrides super class' method.
        
    .self$.checkMassField(mass=mass, mass.field=mass.field)

    ids <- NULL

    # Search by name
    if ( ! is.null(name) && ! is.na(name))
        ids <- .self$searchByName(name)

    # Search by mass
    if ( ! is.null(mass)) {

        mass.field <- .self$getBiodb()$getEntryFields()$getRealName(mass.field)

        if ( ! mass.field %in% c('monoisotopic.mass' ,'molecular.mass'))
            .self$caution('Mass field "', mass.field, '" is not handled.')

        else {

            if (mass.tol.unit == 'ppm') {
                mass.min <- mass * (1 - mass.tol * 1e-6)
                mass.max <- mass * (1 + mass.tol * 1e-6)
            } else {
                mass.min <- mass - mass.tol
                mass.max <- mass + mass.tol
            }

            if (mass.field == 'monoisotopic.mass')
                mass.ids <- .self$wsFindExactMass(mass.min=mass.min,
                                                  mass.max=mass.max,
                                                  retfmt='ids')
            else
                mass.ids <- .self$wsFindMolecularWeight(mass.min=mass.min,
                                                        mass.max=mass.max,
                                                        retfmt='ids')
            .self$debug('Got entry IDs ', paste(mass.ids, collapse=', '), '.')
            if ( ! is.null(mass.ids) && any(! is.na(mass.ids))) {
                mass.ids <- sub('^cpd:', '', mass.ids)
                if (is.null(ids))
                    ids <- mass.ids
                else
                    ids <- ids[ids %in% mass.ids]
            }
        }
    }

    # Cut
    if ( ! is.na(max.results) && max.results > 0 && max.results < length(ids))
        ids <- ids[seq_len(max.results)]

    return(ids)
},

# Get entry image url {{{3
################################################################################

getEntryImageUrl=function(id) {
    # Overrides super class' method.

    fct <- function(x) {
        bu <- .self$getPropValSlot('urls', 'base.url')
        u <- c(bu, 'Fig', 'compound', paste(x, 'gif', sep='.'))
        BiodbUrl(url=u)$toString()
    }

    return(vapply(id, fct, FUN.VALUE=''))
},

# Get pathway IDs per compound {{{3
################################################################################

getPathwayIdsPerCompound=function(id, org, limit=3) {
    ":\n\nGets organism pathways for each compound. This method retrieves for
    each compound the KEGG pathways of the organism in which the compound is
    involved.
    \nid: A character vector of KEGG Compound IDs.
    \norg: The organism in which to search for pathways, as a KEGG organism code
    (3-4 letters code, like 'hsa', 'mmu', ...). See
    https://www.genome.jp/kegg/catalog/org_list.html for a complete list of KEGG
    organism codes.
    \nlimit: The maximum number of modules IDs to retrieve for each compound.
    Set to 0 to disable.
    \nReturned value: A named list of KEGG pathway ID vectors, where the names
    of the list are the compound IDs."

    pathways <- list()

    fac <- .self$getBiodb()$getFactory()
    kegg.enz.conn <- fac$getConn('kegg.enzyme')

    # Loop on all compound ids
    i <- 0
    for (comp.id in id) {

        pws <- NULL

        # Send progress message
        i <- i + 1
        .self$progressMsg(msg='Retrieving pathways of compounds.', index=i,
                          total=length(id), first=(i == 1))

        # Get compound entry
        comp <- .self$getEntry(comp.id)
        if (is.null(comp))
            next

        # Does this compound have a list of pathways?
        if (comp$hasField('kegg.pathway.id')) {

            # Get pathways
            pws <- comp$getFieldValue('kegg.pathway.id')

            # Convert them to specified organism
            kegg.path.conn <- fac$getConn('kegg.pathway')
            pws <- kegg.path.conn$convertToOrgPathways(pws, org=org)
        }

        # Look for enzymes
        else if (comp$hasField('kegg.enzyme.id')) {

            # Get pathways
            enzid <- comp$getFieldValue('kegg.enzyme.id')
            pws <- kegg.enz.conn$getPathwayIds(enzid, org=org)

            # Filter out wrong pathways
            kpc <- fac$getConn('kegg.pathway')
            pws <- pws[kpc$makesRefToEntry(pws, db='kegg.compound',
                                           oid=comp.id, recurse=TRUE)]
        }

        # Record found pathways
        if ( ! is.null(pws)) {
            if (limit > 0 && length(pws) > limit)
                pws <- pws[1:limit]
            pathways[[comp.id]] <- pws
        }
    }

    return(pathways)
},

# Get module IDs per compound {{{3
################################################################################

getModuleIdsPerCompound=function(id, org, limit=3) {
    ":\n\nGets organism modules for each compound. This method retrieves for
    each compound the KEGG modules of the organism in which the compound is
    involved.
    \nid: A character vector of KEGG Compound IDs.
    \norg: The organism in which to search for modules, as a KEGG organism code
    (3-4 letters code, like 'hsa', 'mmu', ...). See
    https://www.genome.jp/kegg/catalog/org_list.html for a complete list of KEGG
    organism codes.
    \nlimit: The maximum number of modules IDs to retrieve for each compound.
    Set to 0 to disable.
    \nReturned value: A named list of KEGG module ID vectors, where the names
    of the list are the compound IDs."

    modules <- list()
    pw <- .self$getBiodb()$getFactory()$getConn('kegg.pathway')

    # Get pathway IDs
    pwids <- .self$getPathwayIdsPerCompound(id=id, org=org)

    # Loop on all compounds
    for (i in pwids) {
        # Retrieve pathway entries for this compound
        pw.entries <- pw$getEntry(i, nulls=FALSE, drop=FALSE)
        modids <- lapply(pw.entries, function(e) e$getFieldValue('kegg.module.id'))
        modids <- unlist(modids)
        modids <- modids[ ! is.na(modids)]
        modids <- unique(modids)
        if (limit > 0 && length(modids) > limit)
            modids <- modids[1:limit]
        modules <- c(modules, list(modids))
    }

    # Set names
    if (length(modules) > 0)
        names(modules) <- names(pwids)

    return(modules)
},

# Get pathway IDs {{{3
################################################################################

getPathwayIds=function(id, org) {
    ":\n\nGets organism pathways. This method retrieves KEGG pathways of the
    specified organism in which the compounds are involved.
    \nid: A character vector of KEGG Compound IDs.
    \norg: The organism in which to search for pathways, as a KEGG organism code
    (3-4 letters code, like 'hsa', 'mmu', ...). See
    https://www.genome.jp/kegg/catalog/org_list.html for a complete list of KEGG
    organism codes.
    \nReturned value: A vector of KEGG pathway IDs.
    "

    pathways <- .self$getPathwayIdsPerCompound(id=id, org=org)
    pathways <- unique(unlist(pathways, use.names=FALSE))

    return(pathways)
},

# Add info {{{3
################################################################################

addInfo=function(x, id.col, org, limit=3, prefix='') {
    ":\n\nAdd informations (as new column appended to the end) to an existing
    data frame containing a column of KEGG Compound IDs.
    \nx: A data frame containing at least one column with Biodb entry IDs
    identified by the parameter `id.col`.
    \nid.col: The name of the column containing IDs inside the input data frame.
    \norg: The organism in which to search for pathways, as a KEGG organism code
    (3-4 letters code, like 'hsa', 'mmu', ...). See
    https://www.genome.jp/kegg/catalog/org_list.html for a complete list of KEGG
    organism codes.
    \nlimit: This is the maximum number of values obtained for each ID, for
    every column added, in case multiple values are obtained. Set to 0 to get
    all values.
    \nprefix: Insert a prefix at the start of name of all new columns.
    \nReturned value: A data frame containing `x` and new columns appended with
    KEGG identifiers and data.
    "

    .self$.assertIs(x, 'data.frame')

    if (ncol(x) > 0) {
        .self$.assertIs(id.col, 'character')
        if ( ! id.col %in% colnames(x))
            .self$error('Column "', id.col,
                        '" was not found inside data frame.')

        # Get ids
        ids <- as.character(x[[id.col]])

        # Get entries
        entries <- .self$getEntry(ids)

        # Add enzyme IDs and reaction IDs
        enzids <- .self$getBiodb()$entriesFieldToVctOrLst(entries,
                                                          field='kegg.enzyme.id',
                                                          limit=limit)
        fields <- c('kegg.enzyme.id', 'kegg.reaction.id')
        y <- .self$getBiodb()$entryIdsToDataframe(enzids, db='kegg.enzyme',
                                                    limit=limit, fields=fields,
                                                    own.id=TRUE)

        # Add pathway info
        pwids <- .self$getPathwayIdsPerCompound(ids, org=org, limit=limit)
        fields <- c('kegg.pathway.id', 'name', 'pathway.class')
        df2 <- .self$getBiodb()$entryIdsToDataframe(pwids, db='kegg.pathway',
                                                    limit=limit, fields=fields,
                                                    own.id=TRUE,
                                                    prefix='kegg.pathway.')
        y <- cbind(y, df2)

        # Add module info
        modids <- .self$getModuleIdsPerCompound(ids, org=org, limit=limit)
        fields <- c('kegg.module.id', 'name')
        df3 <- .self$getBiodb()$entryIdsToDataframe(modids, db='kegg.module',
                                                    limit=limit, fields=fields,
                                                    own.id=TRUE,
                                                    prefix='kegg.module.')
        y <- cbind(y, df3)

        # Rename columns
        if ( ! is.na(prefix) && nchar(prefix) > 0 && length(colnames(y)) > 0)
            colnames(y) <- paste0(prefix, colnames(y))

        # Add info columns
        x <- cbind(x, y)
    }

    return(x)
}

))

