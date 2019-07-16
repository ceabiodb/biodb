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
