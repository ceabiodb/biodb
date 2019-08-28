# vi: fdm=marker ts=4 et cc=80 tw=80

# KeggEntry {{{1
################################################################################

#' @include BiodbTxtEntry.R
KeggEntry=methods::setRefClass("KeggEntry",
    contains='BiodbTxtEntry',

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(...) {

    callSuper(...)
},

# Private methods {{{2
################################################################################

# Get tag lines {{{3
################################################################################

.getTagLines=function(tag, parsed.content) {

    lines <- character()

    # Loop on all lines of parsed content
    in.tag <- FALSE
    for (line in parsed.content) {

        # Match
        regex <- paste0(if (in.tag) "^" else paste0("^", tag), "\\s+(.*)\\s*$")
        g <- stringr::str_match(line, regex)

        # Exit loop
        if (is.na(g[1, 1]) && in.tag)
            break

        # Store line
        if ( ! is.na(g[1, 1])) {
            s <- g[1, 2]
            lines <- c(lines, s)
            in.tag <- TRUE
        }
    }

    return(lines)
},

# Parse multilines field {{{3
################################################################################

.parseMultilinesField=function(field, tag, parsed.content, strip.chars=' ',
                               split.char=' ') {

    # Get tag lines
    lines <- .self$.getTagLines(tag=tag, parsed.content=parsed.content)

    # Split on character
    if ( ! is.na(split.char))
        lines <- unlist(strsplit(lines, paste0(split.char, "+"), perl=TRUE))

    value <- sub(paste0('[', strip.chars, ']+$'), '',
                 sub(paste0('^[', strip.chars, ']+'), '', lines))

    # Set field value
    if (length(value) > 0)
        .self$setFieldValue(field, value)
},

# Parse names {{{2
################################################################################

.parseNames=function(parsed.content, strip.chars=' ;',
                     split.char=NA_character_) {

    .self$.parseMultilinesField(field='name', tag='NAME',
                                parsed.content=parsed.content,
                                strip.chars=strip.chars, split.char=split.char)
},

# Parse module IDs {{{3
################################################################################

.parseModuleIds=function(parsed.content) {
    module.ids <- .self$.getTagLines(tag='MODULE',
                                     parsed.content=parsed.content)
    if (length(module.ids) > 0) {
        module.ids <- sub('^\\s*[A-Za-z_]*(M[0-9]+)\\s+.*$', '\\1', module.ids)
        .self$setFieldValue('kegg.module.id', module.ids)
    }
},

# Parse pathway IDs {{{3
################################################################################

.parsePathwayIds=function(parsed.content) {
    pathway.ids <- .self$.getTagLines(tag='PATHWAY',
                                      parsed.content=parsed.content)
    if (length(pathway.ids) > 0) {
        pathway.ids <- sub('^\\s*([^ ]+)\\s+.*$', '\\1', pathway.ids)
        .self$setFieldValue('kegg.pathway.id', pathway.ids)
    }
},

# Parse compound IDs {{{3
################################################################################

.parseCompoundIds=function(parsed.content) {
    compound.ids <- .self$.getTagLines(tag='COMPOUND',
                                       parsed.content=parsed.content)
    if (length(compound.ids) > 0) {
        compound.ids <- sub('^\\s*(C[0-9]+)\\s+.*$', '\\1', compound.ids)
        .self$setFieldValue('kegg.compound.id', compound.ids)
    }
},

# Parse DB links {{{3
################################################################################

.parseDbLinks=function(parsed.content) {

    abbrev_to_db <- c(
        RN='kegg.reaction.id',
        'NCBI-GeneID'='ncbi.gene.id',
        'UniProt'='uniprot.id',
        'CAS'='cas.id',
        'ExPASy - ENZYME nomenclature database'='expasy.enzyme.id',
        'ChEBI'='chebi.id',
        'LIPIDMAPS'='lipidmaps.structure.id',
        'PubChem'='ncbi.pubchem.comp.id'
    )

    # Extract DB links
    dblinks <- .self$.getTagLines(tag='DBLINKS', parsed.content=parsed.content)

    if (length(dblinks) > 0) {

        # Extract 
        lnks <- stringr::str_match(dblinks, '^([A-Za-z -]+): +(.+)$')
        lnks <- data.frame(db=lnks[, 2], id=lnks[, 3], stringsAsFactors=FALSE)

        # Translate db abbrev to biodb db ID
        fct <- function(x) {
            if (x %in% names(abbrev_to_db))
                abbrev_to_db[[x]]
            else
                NA_character_
        }
        lnks$dbid <- vapply(lnks$db, fct, FUN.VALUE='')

        # Remove unknown databases
        lnks <- lnks[ ! is.na(lnks$dbid), ]

        # Set fields
        for (i in seq_along(lnks[[1]]))
            .self$setFieldValue(lnks[i, 'dbid'], lnks[i, 'id'])
    }
},

# Parse genes IDs {{{3
################################################################################

.parseGenesIds=function(parsed.content) {

    lines <- .self$.getTagLines(tag='GENES', parsed.content=parsed.content)

    if (length(lines) > 0) {
        genes.ids <- character()
        m <- stringr::str_match(lines, "^\\s*([^:]+):\\s*(.*)\\s*$")
        org <- tolower(m[, 2])
        genes <- gsub('\\([^)]+\\)', '', m[, 3], perl=TRUE)
        for (i in seq_along(org)) {
            ids <- strsplit(genes[[i]], ' ')[[1]]
            fct <- function(gene) paste(org[[i]], gene, sep=':')
            genes.ids <- c(genes.ids, vapply(ids, fct, FUN.VALUE=''))
        }
        .self$setFieldValue('kegg.genes.id', genes.ids)
    }
},

# Parse reaction IDs {{{3
################################################################################

.parseReactionIds=function(parsed.content) {
    rids <- .self$.getTagLines(tag='REACTION',
                               parsed.content=parsed.content)
    if (length(rids) > 0) {
        rids <- stringr::str_match_all(rids, '(^|[ +,])(R[0-9]+)')
        rids <- unlist(lapply(rids, function(x) x[,3]))
        .self$setFieldValue('kegg.reaction.id', rids)
    }
}

))
