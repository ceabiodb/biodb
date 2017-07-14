# vi: fdm=marker

#' @include TxtEntry.R

# Class declaration {{{1
################################################################

MassbankEntry <- methods::setRefClass("MassbankEntry", contains = "TxtEntry")

# Constructor {{{1
################################################################

MassbankEntry$methods( initialize = function(...) {

	callSuper(...)

	.self$addParsingExpression(BIODB.ACCESSION, "^ACCESSION: (.+)$")
	.self$addParsingExpression(BIODB.MSDEV, "^AC\\$INSTRUMENT: (.+)$")
	.self$addParsingExpression(BIODB.MSDEVTYPE, "^AC\\$INSTRUMENT_TYPE: (.+)$")
	.self$addParsingExpression(BIODB.MSTYPE, "^AC\\$MASS_SPECTROMETRY: MS_TYPE (.+)$")
	.self$addParsingExpression(BIODB.NB.PEAKS, "^PK\\$NUM_PEAK: ([0-9]+)$")
	.self$addParsingExpression(BIODB.MSPRECANNOT, "^MS\\$FOCUSED_ION: PRECURSOR_TYPE (.+)$")
	.self$addParsingExpression(BIODB.INCHI, "^CH\\$IUPAC:\\s+(.+)$")
	.self$addParsingExpression(BIODB.INCHIKEY, "^CH\\$LINK: INCHIKEY\\s+(.+)$")
	.self$addParsingExpression(BIODB.CHEMSPIDER.ID, "^CH\\$LINK: CHEMSPIDER\\s+(.+)$")
	.self$addParsingExpression(BIODB.CHEBI.ID, "^CH\\$LINK: CHEBI\\s+(.+)$")
	.self$addParsingExpression(BIODB.KEGG.COMPOUND.ID, "^CH\\$LINK: KEGG\\s+(.+)$")
	.self$addParsingExpression(BIODB.CAS.ID, "^CH\\$LINK: CAS\\s+(.+)$")
	.self$addParsingExpression(BIODB.NCBI.PUBCHEM.COMP.ID, "^CH\\$LINK: PUBCHEM\\s+((CID:)?[0-9]+)")
	.self$addParsingExpression(BIODB.NCBI.PUBCHEM.SUBST.ID, "^CH\\$LINK: PUBCHEM\\s+.*(SID:[0-9]+)")
	.self$addParsingExpression(BIODB.HMDB.METABOLITE.ID, "^CH\\$LINK: HMDB\\s+(HMDB[0-9]+)")
	.self$addParsingExpression(BIODB.FORMULA, "^CH\\$FORMULA:\\s+(.+)$")
	.self$addParsingExpression(BIODB.SMILES, "^CH\\$SMILES:\\s+(.+)$")
	.self$addParsingExpression(BIODB.MASS, "^CH\\$EXACT_MASS:\\s+(.+)$")
	.self$addParsingExpression(BIODB.MSMODE, "^AC\\$MASS_SPECTROMETRY: ION_MODE (.+)$")
	.self$addParsingExpression(BIODB.SYNONYMS, "^CH\\$NAME:\\s+(.+)$")
})

# Parse peak table {{{1
################################################################

MassbankEntry$methods( .parsePeakTable = function(parsed.content) {

	# Parse peaks
	g <- stringr::str_match(parsed.content, "^PK\\$PEAK: (.*)$")
	peak.header.line.number <- which(! is.na(g[, 2]))
	if (length(peak.header.line.number) == 0)
		return # No peaks
	peaks <- data.frame(stringsAsFactors = FALSE)
	peak.header <- g[peak.header.line.number, 2]
	cols <- strsplit(peak.header, ' ')[[1]]

	# Build parsing expression
	regex <- '^'
	col.desc <- list('m/z'      = list(name = BIODB.PEAK.MZ, type = 'double', regex = '([0-9][0-9.]*)'),
	                 'int.'     = list(name = BIODB.PEAK.INTENSITY, type = 'double', regex = '([0-9][0-9.]*)'),
	                 'rel.int.' = list(name = BIODB.PEAK.RELATIVE.INTENSITY, type = 'integer', regex = '([0-9]+)'))
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

	# Parse peaks
	i <- 1
	while ( peak.header.line.number + i <= length(parsed.content)) {

		# Parse line
		g <- stringr::str_match(parsed.content[[peak.header.line.number + i]], regex)
		match <- ! is.na(g[1, 1])
		if (match)
			peaks[i, ] <- g[1, 2:(length(cols)+1), drop = TRUE]
		else
			break

		# Next line
		i <- i + 1
	}

	# Set new peaks table
	.self$setFieldValue(BIODB.PEAKS, peaks)

	# Check number of peaks
	if (.self$hasField(BIODB.PEAKS) && .self$getFieldValue(BIODB.NB.PEAKS, compute = FALSE) != nrow(.self$getFieldValue(BIODB.PEAKS, compute = FALSE)))
	   	 .self$message(MSG.CAUTION, paste("Found ", nrow(.self$getFieldValue(BIODB.PEAKS, compute = FALSE)), " peak(s) instead of ", .self$getFieldValue(BIODB.NB.PEAKS, compute = FALSE), ' for entry ', .self$getFieldValue(BIODB.ACCESSION), ".", sep = ''))
})

# Parse peak table {{{1
################################################################

MassbankEntry$methods( .parseAnnotationTable = function(parsed.content) {

	# Parse annotations
	g <- stringr::str_match(parsed.content, "^PK\\$ANNOTATION: (.*)$")
	annot.header.line.number <- which(! is.na(g[, 2]))
	if (length(annot.header.line.number) == 0)
		return() # No annotation
	annots <- data.frame(stringsAsFactors = FALSE)
	annot.header <- g[annot.header.line.number, 2]
	cols <- strsplit(annot.header, ' ')[[1]]

	# Build parsing expression
	regex <- '^'
	col.desc <- list('m/z'                  = list(name = BIODB.PEAK.MZ,                type = 'double',    regex = '([0-9][0-9.]*)'),
	                 'struct.'              = list(name = 'struct.',                    type = 'integer',   regex = '([0-9]+)'),
	                 'num'                  = list(name = 'num',                        type = 'integer',   regex = '([0-9]+)'),
	                 'formula'              = list(name = BIODB.PEAK.FORMULA,           type = 'character', regex = '([^ ]+)'),
	                 'tentative_formula'    = list(name = 'tentative.formula',          type = 'character', regex = '([^ ]+)'),
	                 'formula_count'        = list(name = BIODB.PEAK.FORMULA.COUNT,     type = 'integer',   regex = '([0-9]+)'),
	                 'error(ppm)'           = list(name = BIODB.PEAK.ERROR.PPM,         type = 'double',    regex = '([0-9][0-9.]*)'),
	                 'mass'                 = list(name = BIODB.PEAK.MASS,              type = 'double',    regex = '([0-9][0-9.]*)')
	                 )
	for (c in cols) {
		if (c %in% names(col.desc)) {
			regex <- paste(regex, col.desc[[c]]$regex, sep = '\\s+')
			annots[col.desc[[c]]$name] <- vector(mode = col.desc[[c]]$type)
		}
		else {
			regex <- paste(regex, '([^ ]+)', sep = '\\s+')
			annots[c] <- character()
		}
	}
	regex <- paste(regex, '$', sep = '')

	# Concatenate annotations spread on several lines
	i <- annot.header.line.number + 1
	while (i <= length(parsed.content)) {

		# Annotation block?
		if (length(grep('^  ', parsed.content[[i]])) == 0)
			break # Not an annotation block => leave

		# Is next line the suite of the current line?
		while (i + 1 <= length(parsed.content) && length(grep('^    ', parsed.content[[i + 1]])) == 1) {
			parsed.content[[i]] <- paste(parsed.content[[i]], parsed.content[[i + 1]], sep = '')
			parsed.content <- if (i + 2 <= length(parsed.content)) parsed.content[c(1:i,i+2:length(parsed.content))] else parsed.content[c(1:i)]
		}

		# Next line
		i <- i + 1
	}

	# Parse annotations
	i <- 1
	while (annot.header.line.number + i <= length(parsed.content)) {

		# Parse line
		g <- stringr::str_match(parsed.content[[annot.header.line.number + i]], regex)
		match <- ! is.na(g[1, 1])
		if (match)
			annots[i, ] <- g[1, 2:(length(cols)+1), drop = TRUE]
		else
			break

		# Next line
		i <- i + 1
	}

	# Merge peaks and annotations
	if (.self$hasField(BIODB.PEAKS))
		.self$setFieldValue(BIODB.PEAKS, merge(.self$getFieldValue(BIODB.PEAKS, compute = FALSE), annots, all.x = TRUE))

	# Check number of peaks
	if (.self$hasField(BIODB.PEAKS) && .self$getFieldValue(BIODB.NB.PEAKS, compute = FALSE) != nrow(.self$getFieldValue(BIODB.PEAKS, compute = FALSE)))
	   	 .self$message(MSG.CAUTION, paste("Found ", nrow(.self$getFieldValue(BIODB.PEAKS, compute = FALSE)), " peak(s) instead of ", .self$getFieldValue(BIODB.NB.PEAKS, compute = FALSE), ' for entry ', .self$getFieldValue(BIODB.ACCESSION), ".", sep = ''))
})

# Parse fields after {{{1
################################################################

MassbankEntry$methods( .parseFieldsAfter = function(parsed.content) {

	# List of precursors
	g <- stringr::str_match(parsed.content, "^MS\\$FOCUSED_ION: PRECURSOR_M/Z ([0-9./]+)$")
	results <- g[ ! is.na(g[,1]), , drop = FALSE]
	if (nrow(results) > 0) {
		precursors <- strsplit(results[,2], '/', fixed = TRUE)[[1]]
		.self$setFieldValue(BIODB.MSPRECMZ, precursors)
	}

	# Retention time
	g <- stringr::str_match(parsed.content, "^AC\\$CHROMATOGRAPHY: RETENTION_TIME\\s+([0-9.]+)\\s+([minsec]+)\\s*.*$")
	results <- g[ ! is.na(g[,1]), , drop = FALSE]
	if (nrow(results) > 0) {
		unit <- tolower(results[,3]) 
		if ( ! unit %in% c('min', 'sec', 's'))
			.self$message(MSG.WARNING, paste("Unknown unit", unit, " for retention time while parsing massbank entry."))
		rt <- as.numeric(results[,2])
		if (unit == 'min')
			rt <- 60 * rt
		.self$setFieldValue(BIODB.CHROM.COL.RT, rt)
	}

	# Name
	if (.self$hasField(BIODB.SYNONYMS)) {
		v <- .self$getFieldValue(BIODB.SYNONYMS, compute = FALSE)
		if (length(v) > 0) {
			.self$setFieldValue(BIODB.NAME, v[[1]])
			if (length(v) == 1)
				.self$removeField(BIODB.SYNONYMS)
			else
				.self$setFieldValue(BIODB.SYNONYMS, v[2:length(v)])
		}
	}

	# MS mode
	if (.self$hasField(BIODB.MSMODE))
		.self$setFieldValue(BIODB.MSMODE, if (.self$getFieldValue(BIODB.MSMODE, compute = FALSE) == 'POSITIVE') BIODB.MSMODE.POS else BIODB.MSMODE.NEG)

	# MS level
	if (.self$hasField(BIODB.MSTYPE)) {
		mstype = .self$getFieldValue(BIODB.MSTYPE)
		ms.level = strtoi(sub('^MS([0-9])$', '\\1', mstype, perl = TRUE))
		if (is.na(ms.level) && mstype == 'MS')
			ms.level = 1

		if (is.na(ms.level)) 
			.self$message(MSG.ERROR, paste("Impossible to parse MS level of Massbank entry ", .self$getFieldValue(BIODB.ACCESSION), ".", sep = ''))
		.self$setFieldValue(BIODB.MS.LEVEL, ms.level)
	}
	
	# Parsing of peak table
	.self$.parsePeakTable(parsed.content)
	
	# Parsing of annotation table
	.self$.parseAnnotationTable(parsed.content)
})
