MyMyKeggEntry=methods::setRefClass("KeggEntry",
    contains='BiodbTxtEntry',

methods=list(

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

# Parse names {{{3
################################################################################

.parseNames=function(parsed.content, strip.chars=' ;',
                     split.char=NA_character_) {

    .self$.parseMultilinesField(field='name', tag='NAME',
                                parsed.content=parsed.content,
                                strip.chars=strip.chars, split.char=split.char)
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

        # Split Uniprot IDs
        if (.self$hasField('uniprot.id')) {
            ids <- strsplit(.self$getFieldValue('uniprot.id'), ' +', perl=TRUE)[[1]]
            .self$setFieldValue('uniprot.id', ids)
        }
    }
},

