# vi: fdm=marker ts=4 et cc=80

#' @include BiodbTxtEntry.R

# Class declaration {{{1
################################################################################

KeggEntry = methods::setRefClass("KeggEntry", contains = 'BiodbTxtEntry')

# Initialize {{{1
################################################################################

KeggEntry$methods( initialize = function(...) {

    callSuper(...)
})

# Get tag lines {{{1
################################################################################

KeggEntry$methods( .getTagLines = function(tag, parsed.content) {

    lines = character()

    # Loop on all lines of parsed content
    in.tag = FALSE
    for (line in parsed.content) {

        # Match
        regex = paste0(if (in.tag) "^" else paste0("^", tag), "\\s+(.*)\\s*$")
        g <- stringr::str_match(line, regex)

        # Exit loop
        if (is.na(g[1, 1]) && in.tag)
            break

        # Store line
        if ( ! is.na(g[1, 1])) {
            s = g[1, 2]
            lines = c(lines, s)
            in.tag = TRUE
        }
    }

    return(lines)
})

# Parse multilines field {{{1
################################################################################

KeggEntry$methods( .parseMultilinesField = function(field, tag, parsed.content, strip.chars = ' ', split.char = ' ') {

    # Get tag lines
    lines = .self$.getTagLines(tag = tag, parsed.content = parsed.content)

    # Split on character
    if ( ! is.na(split.char))
        lines = unlist(strsplit(lines, paste0(split.char, "+"), perl = TRUE))

    value = sub(paste0('[', strip.chars, ']+$'), '', sub(paste0('^[', strip.chars, ']+'), '', lines))

    # Set field value
    if (length(value) > 0)
        .self$setFieldValue(field, value)
})

# Parse module IDs {{{1
################################################################################

KeggEntry$methods( .parseModuleIds = function(parsed.content) {
    module.ids = .self$.getTagLines(tag = 'MODULE', parsed.content = parsed.content)
    if (length(module.ids) > 0) {
        module.ids = sub('^\\s*[A-Za-z_]*(M[0-9]+)\\s+.*$', '\\1', module.ids)
        .self$setFieldValue('kegg.module.id', module.ids)
    }
})

# Parse pathway IDs {{{1
################################################################################

KeggEntry$methods( .parsePathwayIds = function(parsed.content) {
    pathway.ids = .self$.getTagLines(tag = 'PATHWAY', parsed.content = parsed.content)
    if (length(pathway.ids) > 0) {
        pathway.ids = sub('^\\s*([^ ]+)\\s+.*$', '\\1', pathway.ids)
        .self$setFieldValue('kegg.pathway.id', pathway.ids)
    }
})

# Parse compound IDs {{{1
################################################################################

KeggEntry$methods( .parseCompoundIds = function(parsed.content) {
    compound.ids = .self$.getTagLines(tag = 'COMPOUND', parsed.content = parsed.content)
    if (length(compound.ids) > 0) {
        compound.ids = sub('^\\s*(C[0-9]+)\\s+.*$', '\\1', compound.ids)
        .self$setFieldValue('kegg.compound.id', compound.ids)
    }
})

# Parse reaction IDs {{{1
################################################################################

KeggEntry$methods( .parseReactionIds = function(parsed.content) {
    reaction.ids = .self$.getTagLines(tag = 'REACTION', parsed.content = parsed.content)
    if (length(reaction.ids) > 0) {
        reaction.ids = stringr::str_match_all(reaction.ids, '(^|[ +,])(R[0-9]+)')
        reaction.ids = unlist(lapply(reaction.ids, function(x) x[,3]))
        .self$setFieldValue('kegg.reaction.id', reaction.ids)
    }
})
