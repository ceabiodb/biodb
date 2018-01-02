# vi: fdm=marker

#' @include TxtEntry.R

# Class declaration {{{1
################################################################

MassbankEntry <- methods::setRefClass("MassbankEntry", contains = "TxtEntry")

# Do parse content {{{1
################################################################

MassbankEntry$methods( .doParseContent = function(content) {

	# Get lines of content
	lines <- strsplit(content, "\r?\n")[[1]]

	# Look for last line
	g <- stringr::str_match(lines, "^//$")
	last.line.number <- which(! is.na(g[, 1]))

	# More than one last line marker?
	if (length(last.line.number) > 1) {

		# Get accession number
		g <- stringr::str_match(lines, "^ACCESSION: (.+)$")
		accession <- g[ ! is.na(g[,1]), 2, drop = FALSE]
		if (length(accession) > 1)
			accession <- accession[[1]]

		# Message
		.self$message('caution', paste("More than one last line marker found in entry ", accession, ", at lines ", paste(last.line.number, collapse = ", "), ". All lines after the first last line marker have been deleted.", sep = ''))

		# Remove all lines after the first marker
		lines <- lines[1:last.line.number[[1]]]
	}

	return(lines)
})

# Is parsed content correct {{{1
################################################################

MassbankEntry$methods( .isParsedContentCorrect = function(parsed.content) {

	# Look for last line
	g <- stringr::str_match(parsed.content, "^//$")
	last.line.number <- which(! is.na(g[, 1]))

	# Incorrect last line?
	if (length(last.line.number) != 1 || last.line.number != length(parsed.content)) {

		# Get accession number
		g <- stringr::str_match(parsed.content, "^ACCESSION: (.+)$")
		accession <- g[ ! is.na(g[,1]), 2, drop = FALSE]
		if (length(accession) > 1)
			accession <- accession[[1]]

		# Too much last lines
		if (length(last.line.number) > 1)
			.self$message('caution', paste("More than one last line marker found in entry ", accession, ", at lines ", paste(last.line.number, collapse = ", "), ".", sep = ''))

		# No last line
		else if (length(last.line.number) == 0)
			.self$message('caution', paste("No last line symbol (\"//\") found in entry ", accession, ".", sep = ''))

		# Last line symbol no at last line
		else if (last.line.number != length(parsed.content))
			.self$message('caution', paste("Last line symbol (\"//\") found at line ", last.line.number, " in entry ", accession, " instead of last line.", sep = ''))

		return(FALSE)
	}

	return(TRUE)
})

# Constructor {{{1
################################################################

MassbankEntry$methods( initialize = function(...) {

	callSuper(...)
	.self$.abstract.class('MassbankEntry')

	.self$addParsingExpression('ACCESSION', "^ACCESSION: (.+)$")
	.self$addParsingExpression('MSDEV', "^AC\\$INSTRUMENT: (.+)$")
	.self$addParsingExpression('MSDEVTYPE', "^AC\\$INSTRUMENT_TYPE: (.+)$")
	.self$addParsingExpression('MSTYPE', "^AC\\$MASS_SPECTROMETRY: MS_TYPE (.+)$")
	.self$addParsingExpression('NB.PEAKS', "^PK\\$NUM_PEAK: ([0-9]+)$")
	.self$addParsingExpression('MSPRECANNOT', "^MS\\$FOCUSED_ION: PRECURSOR_TYPE (.+)$")
	.self$addParsingExpression('INCHI', "^CH\\$IUPAC:\\s+(.+)$")
	.self$addParsingExpression('INCHIKEY', "^CH\\$LINK: INCHIKEY\\s+(.+)$")
	.self$addParsingExpression('chemspider.id', "^CH\\$LINK: CHEMSPIDER\\s+(.+)$")
	.self$addParsingExpression('chebi.id', "^CH\\$LINK: CHEBI\\s+(.+)$")
	.self$addParsingExpression('kegg.compound.id', "^CH\\$LINK: KEGG\\s+(.+)$")
	.self$addParsingExpression('CAS.ID', "^CH\\$LINK: CAS\\s+(.+)$")
	.self$addParsingExpression('ncbi.pubchem.comp.id', "^CH\\$LINK: PUBCHEM\\s+((CID:)?[0-9]+)")
	.self$addParsingExpression('ncbi.pubchem.subst.id', "^CH\\$LINK: PUBCHEM\\s+.*(SID:[0-9]+)")
	.self$addParsingExpression('hmdb.metabolites.id', "^CH\\$LINK: HMDB\\s+(HMDB[0-9]+)")
	.self$addParsingExpression('FORMULA', "^CH\\$FORMULA:\\s+(.+)$")
	.self$addParsingExpression('SMILES', "^CH\\$SMILES:\\s+(.+)$")
	.self$addParsingExpression('exact.mass', "^CH\\$EXACT_MASS:\\s+(.+)$")
	.self$addParsingExpression('MSMODE', "^AC\\$MASS_SPECTROMETRY: ION_MODE (.+)$")
	.self$addParsingExpression('SYNONYMS', "^CH\\$NAME:\\s+(.+)$")
})

# Parse peak info {{{1
################################################################

MassbankEntry$methods( .parsePeakInfo = function(parsed.content, title) {

	# Parse peaks
	g <- stringr::str_match(parsed.content, paste("^PK\\$", title, ": (.*)$", sep = ''))
	peak.header.line.number <- which(! is.na(g[, 2]))
	if (length(peak.header.line.number) == 0)
		return() # No peaks
	peaks <- data.frame(stringsAsFactors = FALSE)
	peak.header <- g[peak.header.line.number, 2]
	cols <- strsplit(peak.header, ' ')[[1]]

	# Build parsing expression
	regex <- '^'
	col.desc <- list('m/z'                  = list(name = 'peak.mz', type = 'double', regex = '([0-9][0-9.]*)'),
	                 'int.'                 = list(name = 'peak.intensity', type = 'double', regex = '([0-9][0-9.e]*)'),
	                 'rel.int.'             = list(name = 'peak.relative.intensity', type = 'integer', regex = '([0-9]+)'),
	                 'struct.'              = list(name = 'struct.',                    type = 'integer',   regex = '([0-9]+)'),
	                 'num'                  = list(name = 'num',                        type = 'integer',   regex = '([0-9]+)'),
	                 'formula'              = list(name = 'peak.formula',           type = 'character', regex = '([^ ]+)'),
	                 'tentative_formula'    = list(name = 'tentative.formula',          type = 'character', regex = '([^ ]+)'),
	                 'formula_count'        = list(name = 'peak.formula.count',     type = 'integer',   regex = '([0-9]+)'),
	                 'error(ppm)'           = list(name = 'peak.error.ppm',         type = 'double',    regex = '([0-9][0-9.]*)'),
	                 'mass'                 = list(name = 'peak.mass',              type = 'double',    regex = '([0-9][0-9.]*)')
	                 )
	for (c in cols) {
		if (c %in% names(col.desc)) {
			regex <- paste(regex, col.desc[[c]]$regex, sep = '\\s+')
			peaks[col.desc[[c]]$name] <- vector(mode = col.desc[[c]]$type)
		}
		else {
			regex <- paste(regex, '([^ ]+)', sep = '\\s+')
			peaks[c] <- character()
		}
	}
	regex <- paste(regex, '$', sep = '')

	# Concatenate info spread on several lines
	i <- peak.header.line.number + 1
	while (i <= length(parsed.content)) {

		# Annotation block?
		if (length(grep('^  ', parsed.content[[i]])) == 0)
			break # Not an peakation block => leave

		# Is next line the suite of the current line?
		while (i + 1 <= length(parsed.content) && length(grep('^    ', parsed.content[[i + 1]])) == 1) {
			parsed.content[[i]] <- paste(parsed.content[[i]], parsed.content[[i + 1]], sep = '')
			parsed.content <- if (i + 2 <= length(parsed.content)) parsed.content[c(1:i,i+2:length(parsed.content))] else parsed.content[c(1:i)]
		}

		# Next line
		i <- i + 1
	}

	# Parse peaks
	i <- 1
	while ( peak.header.line.number + i <= length(parsed.content)) {

		# Parse line
		g <- stringr::str_match(parsed.content[[peak.header.line.number + i]], regex)
		match <- ! is.na(g[1, 1])
		if (match) {
			for (j in seq(ncol(peaks)))
				peaks[i, j] <- as.vector(g[1, j + 1], mode = class(peaks[[j]]))
		}
		else
			break

		# Next line
		i <- i + 1
	}

	# Merge with existing peak info
	if (.self$hasField('PEAKS'))
		.self$setFieldValue('PEAKS', merge(.self$getFieldValue('PEAKS', compute = FALSE), peaks, all.x = TRUE))
	else
		# Set new peaks table
		.self$setFieldValue('PEAKS', peaks)

	# Check number of peaks
	if (.self$hasField('PEAKS') && .self$getFieldValue('NB.PEAKS', compute = FALSE) != nrow(.self$getFieldValue('PEAKS', compute = FALSE)))
	   	 .self$message('caution', paste("Found ", nrow(.self$getFieldValue('PEAKS', compute = FALSE)), " peak(s) instead of ", .self$getFieldValue('NB.PEAKS', compute = FALSE), ' for entry ', .self$getFieldValue('ACCESSION'), ".", sep = ''))
})

# Parse peak table {{{1
################################################################

MassbankEntry$methods( .parsePeakTable = function(parsed.content) {
	.self$.parsePeakInfo(parsed.content, title = 'PEAK')
})

# Parse annotation table {{{1
################################################################

MassbankEntry$methods( .parseAnnotationTable = function(parsed.content) {
	.self$.parsePeakInfo(parsed.content, title = 'ANNOTATION')
})

# Parse fields after {{{1
################################################################

MassbankEntry$methods( .parseFieldsAfter = function(parsed.content) {

	# PubChem IDs when SID and CID are both specified in that order
	g <- stringr::str_match(parsed.content, "^CH\\$LINK: PUBCHEM\\s+(SID:[0-9]+)\\s+(CID:[0-9]+)")
	results <- g[ ! is.na(g[,1]), , drop = FALSE]
	if (nrow(results) > 0) {
		sid <- results[,2]
		cid <- results[,3]
		.self$setFieldValue('ncbi.pubchem.subst.id', sid)
		.self$setFieldValue('ncbi.pubchem.comp.id', cid)
	}

	# List of precursors
	g <- stringr::str_match(parsed.content, "^MS\\$FOCUSED_ION: PRECURSOR_M/Z ([0-9./]+)$")
	results <- g[ ! is.na(g[,1]), , drop = FALSE]
	if (nrow(results) > 0) {
		precursors <- strsplit(results[,2], '/', fixed = TRUE)[[1]]
		.self$setFieldValue('MSPRECMZ', precursors)
	}

	# Retention time
	g <- stringr::str_match(parsed.content, "^AC\\$CHROMATOGRAPHY: RETENTION_TIME\\s+([0-9.]+)\\s+([minsec]+)\\s*.*$")
	results <- g[ ! is.na(g[,1]), , drop = FALSE]
	if (nrow(results) > 0) {
		unit <- tolower(results[,3]) 
		if ( ! unit %in% c('min', 'sec', 's'))
			.self$message('warning', paste("Unknown unit", unit, " for retention time while parsing massbank entry."))
		rt <- as.numeric(results[,2])
		if (unit == 'min')
			rt <- 60 * rt
		.self$setFieldValue('CHROM.COL.RT', rt)
	}

	# Name
	if (.self$hasField('SYNONYMS')) {
		v <- .self$getFieldValue('SYNONYMS', compute = FALSE)
		if (length(v) > 0) {
			.self$setFieldValue('NAME', v[[1]])
			if (length(v) == 1)
				.self$removeField('SYNONYMS')
			else
				.self$setFieldValue('SYNONYMS', v[2:length(v)])
		}
	}

	# MS mode
	if (.self$hasField('MSMODE'))
		.self$setFieldValue('MSMODE', if (.self$getFieldValue('MSMODE', compute = FALSE) == 'POSITIVE') BIODB.MSMODE.POS else BIODB.MSMODE.NEG)

	# MS level
	if (.self$hasField('MSTYPE')) {
		mstype = .self$getFieldValue('MSTYPE')
		ms.level = strtoi(sub('^MS([0-9])$', '\\1', mstype, perl = TRUE))
		if (is.na(ms.level) && mstype == 'MS')
			ms.level = 1

		if (is.na(ms.level)) 
			.self$message('error', paste("Impossible to parse MS level of Massbank entry ", .self$getFieldValue('ACCESSION'), ".", sep = ''))
		.self$setFieldValue('MS.LEVEL', ms.level)
	}
	
	# Parsing of peak table
	.self$.parsePeakTable(parsed.content)
	
	# Parsing of annotation table
	.self$.parseAnnotationTable(parsed.content)
})
