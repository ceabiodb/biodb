###########################
# MASSBANK SPECTRUM CLASS #
###########################

MassbankEntry <- methods::setRefClass("MassbankEntry", contains = "BiodbEntry")

###########
# FACTORY #
###########

createMassbankEntryFromTxt <- function(biodb, contents, drop = TRUE) {

	entries <- list()

	# Define fields regex
	regex <- character()
	regex[[BIODB.ACCESSION]] <- "^ACCESSION: (.+)$"
	regex[[BIODB.MSDEV]] <- "^AC\\$INSTRUMENT: (.+)$"
	regex[[BIODB.MSDEVTYPE]] <- "^AC\\$INSTRUMENT_TYPE: (.+)$"
	regex[[BIODB.MSTYPE]] <- "^AC\\$MASS_SPECTROMETRY: MS_TYPE (.+)$"
	regex[[BIODB.MSPRECMZ]] <- "^MS\\$FOCUSED_ION: PRECURSOR_M/Z (.+)$"
	regex[[BIODB.NB.PEAKS]] <- "^PK\\$NUM_PEAK: ([0-9]+)$"
	regex[[BIODB.MSPRECANNOT]] <- "^MS\\$FOCUSED_ION: PRECURSOR_TYPE (.+)$"
	regex[[BIODB.CHEBI.ID]] <- "^CH\\$LINK: CHEBI\\s+(.+)$"
	regex[[BIODB.KEGGCOMPOUND.ID]] <- "^CH\\$LINK: KEGG\\s+(.+)$"
	regex[[BIODB.INCHI]] <- "^CH\\$IUPAC:\\s+(.+)$"
	regex[[BIODB.INCHIKEY]] <- "^CH\\$LINK: INCHIKEY\\s+(.+)$"
	regex[[BIODB.CHEMSPIDER.ID]] <- "^CH\\$LINK: CHEMSPIDER\\s+(.+)$"
	regex[[BIODB.CAS.ID]] <- "^CH\\$LINK: CAS\\s+(.+)$"
	regex[[BIODB.FORMULA]] <- "^CH\\$FORMULA:\\s+(.+)$"
	regex[[BIODB.SMILES]] <- "^CH\\$SMILES:\\s+(.+)$"
	regex[[BIODB.MASS]] <- "^CH\\$EXACT_MASS:\\s+(.+)$"
	regex[[BIODB.PUBCHEM.COMP.ID]] <- "^CH\\$LINK: PUBCHEM\\s+((CID:)?[0-9]+)"
	regex[[BIODB.PUBCHEM.SUBST.ID]] <- "^CH\\$LINK: PUBCHEM\\s+.*SID:([0-9]+)"
	regex[[BIODB.HMDBMETABOLITE.ID]] <- "^CH\\$LINK: HMDB\\s+(HMDB[0-9]+)"

	n <- 0
	for (text in contents) {

		n <- n +1

		# Create instance
		entry <- MassbankEntry$new(biodb = biodb)

		entry$message(MSG.DEBUG, paste("Parsing content", n, "/", length(contents), "..."))

		if ( ! is.null(text) && ! is.na(text)) {

			# Read text
			lines <- strsplit(text, "\n")
			for (s in lines[[1]]) {

				# Test generic regex
				parsed <- FALSE
				for (field in names(regex)) {
					g <- stringr::str_match(s, regex[[field]])
					if ( ! is.na(g[1,1])) {
						entry$setFieldValue(field, g[1,2])
						parsed <- TRUE
						break
					}
				}
				if (parsed)
					next

				# Retention time
				g <- stringr::str_match(s, "^AC\\$CHROMATOGRAPHY: RETENTION_TIME\\s+([0-9.]+)\\s+([minsec]+)\\s*.*$")
				if ( ! is.na(g[1,1])) {
					unit <- tolower(g[1,3]) 
					if ( ! unit %in% c('min', 'sec', 's'))
						entry$message(MSG.WARNING, paste("Unknown unit", unit, " for retention time while parsing massbank entry."))
					rt <- as.numeric(g[1,2])
					if (unit == 'min')
						rt <- 60 * rt
					entry$setFieldValue(BIODB.CHROM.COL.RT, rt)
					next
				}

				# Name
				if (is.na(entry$getFieldValue(BIODB.NAME))) {
					g <- stringr::str_match(s, "^CH\\$NAME:\\s+(.+)$")
					if ( ! is.na(g[1,1])) {
						entry$setFieldValue(BIODB.NAME, g[1,2])
						next
					}
				}
		
				# PubChem
				g <- stringr::str_match(s, "^CH\\$LINK: PUBCHEM\\s+([0-9]+)$")
				if ( ! is.na(g[1,1])) {
					entry$setFieldValue(BIODB.PUBCHEM.SUBST.ID, g[1,2])
					next
				}

				# MS MODE
				g <- stringr::str_match(s, "^AC\\$MASS_SPECTROMETRY: ION_MODE (.+)$")
				if ( ! is.na(g[1,1])) {
					entry$setFieldValue(BIODB.MSMODE, if (g[1,2] == 'POSITIVE') BIODB.MSMODE.POS else BIODB.MSMODE.NEG)
					next
				}

				# PEAKS
				if (.parse.peak.line(entry, s))
					next
			}
		}

		entries <- c(entries, entry)
	}

	# Replace elements with no accession id by NULL
	entries <- lapply(entries, function(x) if (is.na(x$getFieldValue(BIODB.ACCESSION))) NULL else x)

	# If the input was a single element, then output a single object
	if (drop && length(contents) == 1)
		entries <- entries[[1]]

	return(entries)
}

###################
# PARSE PEAK LINE #
###################

.parse.peak.line <- function(entry, line) {

	peaks <- BIODB.PEAK.DF.EXAMPLE
	
	# Annotation
	g <- stringr::str_match(line, "^\\s+([0-9][0-9.]*) ([A-Z0-9+-]+) ([0-9]+) ([0-9][0-9.]*) ([0-9][0-9.]*)$")
	if ( ! is.na(g[1,1]))
		peaks[1, c(BIODB.PEAK.MZ, BIODB.PEAK.FORMULA, BIODB.PEAK.FORMULA.COUNT, BIODB.PEAK.MASS, BIODB.PEAK.ERROR.PPM)] <- list(as.double(g[1,2]), g[1,3], as.integer(g[1,4]), as.double(g[1,5]), as.double(g[1,6]))

	# Peak
	g <- stringr::str_match(line, "^\\s+([0-9][0-9.]*) ([0-9][0-9.]*) ([0-9]+)$")
	if ( ! is.na(g[1,1]))
		peaks[1, c(BIODB.PEAK.MZ, BIODB.PEAK.INTENSITY, BIODB.PEAK.RELATIVE.INTENSITY)] <- list(as.double(g[1,2]), as.double(g[1,3]), as.integer(g[1,4]))

	if (nrow(peaks) > 0) {

		# Get curent peaks and merge with new peaks
		current.peaks <- entry$getFieldValue(BIODB.PEAKS)
		if ( ! is.null(current.peaks))
			peaks <- rbind(current.peaks, peaks)

		entry$setFieldValue(BIODB.PEAKS, peaks)

		return(TRUE)
	}

	return(FALSE)
}
