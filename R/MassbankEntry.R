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
	.self$addParsingExpression(BIODB.MSPRECMZ, "^MS\\$FOCUSED_ION: PRECURSOR_M/Z (.+)$")
	.self$addParsingExpression(BIODB.NB.PEAKS, "^PK\\$NUM_PEAK: ([0-9]+)$")
	.self$addParsingExpression(BIODB.MSPRECANNOT, "^MS\\$FOCUSED_ION: PRECURSOR_TYPE (.+)$")
	.self$addParsingExpression(BIODB.CHEBI.ID, "^CH\\$LINK: CHEBI\\s+(.+)$")
	.self$addParsingExpression(BIODB.KEGG.COMPOUND.ID, "^CH\\$LINK: KEGG\\s+(.+)$")
	.self$addParsingExpression(BIODB.INCHI, "^CH\\$IUPAC:\\s+(.+)$")
	.self$addParsingExpression(BIODB.INCHIKEY, "^CH\\$LINK: INCHIKEY\\s+(.+)$")
	.self$addParsingExpression(BIODB.CHEMSPIDER.ID, "^CH\\$LINK: CHEMSPIDER\\s+(.+)$")
	.self$addParsingExpression(BIODB.CAS.ID, "^CH\\$LINK: CAS\\s+(.+)$")
	.self$addParsingExpression(BIODB.FORMULA, "^CH\\$FORMULA:\\s+(.+)$")
	.self$addParsingExpression(BIODB.SMILES, "^CH\\$SMILES:\\s+(.+)$")
	.self$addParsingExpression(BIODB.MASS, "^CH\\$EXACT_MASS:\\s+(.+)$")
	.self$addParsingExpression(BIODB.NCBI.PUBCHEM.COMP.ID, "^CH\\$LINK: PUBCHEM\\s+((CID:)?[0-9]+)")
	.self$addParsingExpression(BIODB.NCBI.PUBCHEM.SUBST.ID, "^CH\\$LINK: PUBCHEM\\s+.*SID:([0-9]+)")
	.self$addParsingExpression(BIODB.HMDB.METABOLITE.ID, "^CH\\$LINK: HMDB\\s+(HMDB[0-9]+)")
	.self$addParsingExpression(BIODB.MSMODE, "^AC\\$MASS_SPECTROMETRY: ION_MODE (.+)$")
})

# Parse fields after {{{1
################################################################

MassbankEntry$methods( .parseFieldsAfter = function(parsed.content) {

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
	g <- stringr::str_match(parsed.content, "^CH\\$NAME:\\s+(.+)$")
	results <- g[ ! is.na(g[,1]), , drop = FALSE]
	if (nrow(results) > 0)
		.self$setFieldValue(BIODB.NAME, results[1,2]) # Take first one only

	# MS mode
	if (.self$hasField(BIODB.MSMODE))
		.self$setFieldValue(BIODB.MSMODE, if (.self$getFieldValue(BIODB.MSMODE) == 'POSITIVE') BIODB.MSMODE.POS else BIODB.MSMODE.NEG)

	
	# Annotations
	g <- stringr::str_match(parsed.content, "^\\s+([0-9][0-9.]*) ([A-Z0-9+-]+) ([0-9]+) ([0-9][0-9.]*) ([0-9][0-9.]*)$")
	results <- g[ ! is.na(g[,1]), , drop = FALSE]
	if (nrow(results) > 0) {
		peaks <- BIODB.PEAK.DF.EXAMPLE
		peaks[1:nrow(results), c(BIODB.PEAK.MZ, BIODB.PEAK.FORMULA, BIODB.PEAK.FORMULA.COUNT, BIODB.PEAK.MASS, BIODB.PEAK.ERROR.PPM)] <- list(as.double(results[,2]), results[,3], as.integer(results[,4]), as.double(results[,5]), as.double(results[,6]))
		if (.self$hasField(BIODB.PEAKS))
			peaks <- rbind(.self$getFieldValue(BIODB.PEAKS), peaks)
		else
			.self$setFieldValue(BIODB.PEAKS, peaks)
	}

	# Peaks
	g <- stringr::str_match(parsed.content, "^\\s+([0-9][0-9.]*) ([0-9][0-9.]*) ([0-9]+)$")
	results <- g[ ! is.na(g[,1]), , drop = FALSE]
	if (nrow(results) > 0) {
		peaks <- BIODB.PEAK.DF.EXAMPLE
		peaks[1, c(BIODB.PEAK.MZ, BIODB.PEAK.INTENSITY, BIODB.PEAK.RELATIVE.INTENSITY)] <- list(as.double(g[1,2]), as.double(g[1,3]), as.integer(g[1,4]))
		if (.self$hasField(BIODB.PEAKS))
			peaks <- rbind(.self$getFieldValue(BIODB.PEAKS), peaks)
		else
			.self$setFieldValue(BIODB.PEAKS, peaks)
	}
})
