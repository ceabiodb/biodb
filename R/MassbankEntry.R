# vi: fdm=marker ts=4 et cc=80 tw=80

# MassbankEntry {{{1
################################################################################

#' Massbank entry class.
#'
#' @include BiodbTxtEntry.R
#' @export MassbankEntry
#' @exportClass MassbankEntry
MassbankEntry <- methods::setRefClass("MassbankEntry",
    contains="BiodbTxtEntry",

# Private methods {{{2
################################################################################

methods=list(

# Do parse content {{{3
################################################################################

.doParseContent=function(content) {

    # Get lines of content
    lines <- strsplit(content, "\r?\n")[[1]]

    # Look for last line marker
    g <- stringr::str_match(lines, "^//$")
    last.line.number <- which(! is.na(g[, 1]))

    # More than one last line marker?
    if (length(last.line.number) > 1) {

        # Get accession number
        g <- stringr::str_match(lines, "^ACCESSION: (.+)$")
        accession <- g[ ! is.na(g[,1]), 2, drop=FALSE]
        if (length(accession) > 1)
            accession <- accession[[1]]

        # Message
        .self$caution("More than one last line marker found in entry ",
                      accession, ", at lines ",
                      paste(last.line.number, collapse=", "),
                      ". All lines after the first last line marker have been",
                      " deleted.")

        # Remove all lines after the first marker
        lines <- lines[seq_len(last.line.number[[1]])]
    }

    return(lines)
},

# Is parsed content correct {{{3
################################################################################

.isParsedContentCorrect=function(parsed.content) {

    # Look for last line marker
    g <- stringr::str_match(parsed.content, "^//$")
    last.line.number <- which(! is.na(g[, 1]))

    # Incorrect last line marker?
    if (length(last.line.number) > 1
        || (length(last.line.number) == 1
            && last.line.number != length(parsed.content))) {

        # Get accession number
        g <- stringr::str_match(parsed.content, "^ACCESSION: (.+)$")
        accession <- g[ ! is.na(g[,1]), 2, drop=FALSE]
        if (length(accession) > 1)
            accession <- accession[[1]]

        # Too much last lines
        if (length(last.line.number) > 1)
            .self$caution("More than one last line marker found in entry ",
                          accession, ", at lines ",
                          paste(last.line.number, collapse=", "), ".")

        # No last line
        else if (length(last.line.number) == 0)
            .self$caution("No last line symbol (\"//\") found in entry ",
                          accession, ".")

        # Last line symbol no at last line
        else if (last.line.number != length(parsed.content))
            .self$caution("Last line symbol (\"//\") found at line ",
                          last.line.number, " in entry ", accession,
                          " instead of last line.")

        return(FALSE)
    }

    return(TRUE)
},

# Parse peak info {{{3
################################################################################

.parsePeakInfo=function(parsed.content, title) {

    # Parse peaks
    re <- paste0("^PK\\$", title, ": (.*)$")
    g <- stringr::str_match(parsed.content, re)
    peak.header.line.number <- which(! is.na(g[, 2]))
    if (length(peak.header.line.number) == 0)
        return() # No peaks
    peaks <- data.frame(stringsAsFactors=FALSE)
    peak.header <- g[peak.header.line.number, 2]
    cols <- strsplit(peak.header, ' ')[[1]]

    # Build parsing expression
    regex <- '^'
    col.desc <- list(
        'm/z'=list(name='peak.mz', type='double', regex='([0-9][0-9.]*)'),
        'int.'=list(name='peak.intensity', type='double',
                    regex='([0-9][0-9.e]*)'),
        'rel.int.'=list(name='peak.relative.intensity', type='double',
                        regex='([0-9]+)'),
        'struct.'=list(name='struct.', type='integer', regex='([0-9]+)'),
        'num'=list(name='num', type='integer', regex='([0-9]+)'),
        'formula'=list(name='peak.formula', type='character', regex='([^ ]+)'),
        'tentative_formula'=list(name='tentative.formula', type='character',
                                 regex='([^ ]+)'),
        'formula_count'=list(name='peak.formula.count', type='integer',
                             regex='([0-9]+)'),
        'error(ppm)'=list(name='peak.error.ppm', type='double',
                          regex='(-?[0-9][0-9.]*)'),
        'mass'=list(name='peak.mass', type='double', regex='([0-9][0-9.]*)')
                     )
    for (c in cols) {
        if (c %in% names(col.desc)) {
            regex <- paste(regex, col.desc[[c]]$regex, sep='\\s+')
            peaks[col.desc[[c]]$name] <- vector(mode=col.desc[[c]]$type)
        }
        else {
            regex <- paste(regex, '([^ ]+)', sep='\\s+')
            peaks[c] <- character()
        }
    }
    regex <- paste(regex, '\\s*$', sep='')

    # Concatenate info spread on several lines
    i <- peak.header.line.number + 1
    while (i <= length(parsed.content)) {

        # Annotation block?
        if (length(grep('^  ', parsed.content[[i]])) == 0)
            break # Not an peakation block => leave

        # Is next line the suite of the current line?
        while (i + 1 <= length(parsed.content)
               && length(grep('^    ', parsed.content[[i + 1]])) == 1) {
            parsed.content[[i]] <- paste0(parsed.content[[i]],
                                          parsed.content[[i + 1]])
            r <- seq_len(i)
            if (i + 2 <= length(parsed.content))
                r <- c(r, seq(from=i+2, to=length(parsed.content)))
            parsed.content <- parsed.content[r]
        }

        # Next line
        i <- i + 1
    }

    # Parse peaks
    i <- 1
    while ( peak.header.line.number + i <= length(parsed.content)) {

        # Parse line
        line <- parsed.content[[peak.header.line.number + i]]
        g <- stringr::str_match(line, regex)
        match <- ! is.na(g[1, 1])
        if (match) {
            for (j in seq(ncol(peaks)))
                peaks[i, j] <- as.vector(g[1, j + 1], mode=class(peaks[[j]]))
        }
        else if (substring(line, 1, 2) == '  ')
            .self$caution('Impossible to parse peak line: "', line,
                          '" with regex "', regex, '".')
        else
            break

        # Next line
        i <- i + 1
    }

    # Scale relative intensity to percentage
    if ('peak.relative.intensity' %in% names(peaks)) {

        # Check that at least one peak have 999 as relative intensity
        if (all(peaks[['peak.relative.intensity']] != 999))
            .self$caution("No peak has a relative intensity of 999 inside",
                          " Massbank entry ", .self$getFieldValue('accession'),
                          ".")

        # Scale to percentage
        rint <- peaks[['peak.relative.intensity']]
        peaks[['peak.relative.intensity']] <- rint * 100 / 999
    }

    # Check number of peaks
    if (title == 'PEAK' && .self$hasField('nb.peaks')
        && .self$getFieldValue('nb.peaks', compute=FALSE) != nrow(peaks))
         .self$caution("Found ", nrow(peaks), " peak(s) instead of ",
                       .self$getFieldValue('nb.peaks', compute=FALSE),
                       ' for entry ', .self$getFieldValue('accession'), ".")

    # Merge with existing peak info
    if (.self$hasField('peaks'))
        peaks <- merge(.self$getFieldValue('peaks', compute=FALSE), peaks,
                       all.x=TRUE)

    # Set new peaks table
    .self$setFieldValue('peaks', peaks)
},

# Parse peak table {{{3
################################################################################

.parsePeakTable=function(parsed.content) {
    .self$.parsePeakInfo(parsed.content, title='PEAK')
},

# Parse annotation table {{{3
################################################################################

.parseAnnotationTable=function(parsed.content) {
    .self$.parsePeakInfo(parsed.content, title='ANNOTATION')
},

# Parse fields step 2 {{{3
################################################################################

.parseFieldsStep2=function(parsed.content) {

    # PubChem IDs when SID and CID are both specified in that order
    re <- "^CH\\$LINK: PUBCHEM\\s+(SID:[0-9]+)\\s+(CID:[0-9]+)"
    g <- stringr::str_match(parsed.content, re)
    results <- g[ ! is.na(g[,1]), , drop=FALSE]
    if (nrow(results) > 0) {
        sid <- results[,2]
        cid <- results[,3]
        .self$setFieldValue('ncbi.pubchem.subst.id', sid)
        .self$setFieldValue('ncbi.pubchem.comp.id', cid)
    }

    # List of precursors
    re <- "^MS\\$FOCUSED_ION: PRECURSOR_M/Z ([0-9./]+)$"
    g <- stringr::str_match(parsed.content, re)
    results <- g[ ! is.na(g[,1]), , drop=FALSE]
    if (nrow(results) > 0) {
        precursors <- strsplit(results[,2], '/', fixed=TRUE)[[1]]
        .self$setFieldValue('msprecmz', precursors)
    }

    # Chromatographic column id
    if (.self$hasField('chrom.col.name'))
        .self$setFieldValue('chrom.col.id',
                            .self$getFieldValue('chrom.col.name'))

    # Retention time
    re <- paste0("^AC\\$CHROMATOGRAPHY: RETENTION_TIME\\s+([0-9.]+)",
                 "\\s+([minsec]+)\\s*.*$")
    g <- stringr::str_match(parsed.content, re)
    results <- g[ ! is.na(g[,1]), , drop=FALSE]
    if (nrow(results) > 0) {
        unit <- tolower(results[,3]) 
        if ( ! unit %in% c('min', 'sec', 's'))
            .self$warning("Unknown unit ", unit, " for retention time while",
                          " parsing massbank entry.")
        rt <- as.numeric(results[,2])
        .self$setFieldValue('chrom.rt.unit', if (unit == 'min') 'min' else 's')
        .self$setFieldValue('chrom.rt', rt)
    }

    # MS level
    if (.self$hasField('MSTYPE')) {
        mstype <- .self$getFieldValue('MSTYPE')
        ms.level <- strtoi(sub('^MS([0-9])$', '\\1', mstype, perl=TRUE))
        if (is.na(ms.level) && mstype == 'MS')
            ms.level <- 1

        if (is.na(ms.level)) 
            .self$error("Impossible to parse MS level of Massbank entry ",
                        .self$getFieldValue('ACCESSION'), ".")
        .self$setFieldValue('ms.level', ms.level)
    }
    
    # Parsing of peak table
    .self$.parsePeakTable(parsed.content)
    
    # Parsing of annotation table
    .self$.parseAnnotationTable(parsed.content)

    # Check essential fields
    for (field in c('ms.level'))
        if ( ! .self$hasField(field))
            .self$caution("Massbank entry ", .self$getFieldValue('ACCESSION'),
                          " has no field ", field, ".")
}

))
