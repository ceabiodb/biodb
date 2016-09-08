if ( ! exists('MassbankSpectrum')) { # Do not load again if already loaded

	source('BiodbEntry.R')
	source('MassbankCompound.R')

	###########################
	# MASSBANK SPECTRUM CLASS #
	###########################
	
	MassbankSpectrum <- setRefClass("MassbankSpectrum", contains = "BiodbEntry")

	###########
	# FACTORY #
	###########
	
	createMassbankSpectrumFromTxt <- function(contents, drop = TRUE) {

		library(stringr)

		spectra <- list()

		# Define fields regex
		regex <- character()
		regex[[BIODB.ACCESSION]] <- "^ACCESSION: (.+)$"
		regex[[BIODB.MSDEV]] <- "^AC\\$INSTRUMENT: (.+)$"
		regex[[BIODB.MSDEVTYPE]] <- "^AC\\$INSTRUMENT_TYPE: (.+)$"
		regex[[BIODB.MSTYPE]] <- "^AC\\$MASS_SPECTROMETRY: MS_TYPE (.+)$"
		regex[[BIODB.MSPRECMZ]] <- "^MS\\$FOCUSED_ION: PRECURSOR_M/Z (.+)$"
		regex[[BIODB.NB.PEAKS]] <- "^PK\\$NUM_PEAK: ([0-9]+)$"
		regex[[BIODB.MSPRECANNOT]] <- "^MS\\$FOCUSED_ION: PRECURSOR_TYPE (.+)$"

		for (text in contents) {

			# Create instance
			spectrum <- MassbankSpectrum$new()

			if ( ! is.null(text) && ! is.na(text)) {

				# Read text
				lines <- strsplit(text, "\n")
				for (s in lines[[1]]) {

					# Test generic regex
					parsed <- FALSE
					for (field in names(regex)) {
						g <- str_match(s, regex[[field]])
						if ( ! is.na(g[1,1])) {
							spectrum$setField(field, g[1,2])
							parsed <- TRUE
							break
						}
					}
					if (parsed)
						next

					# MS MODE
					g <- str_match(s, "^AC\\$MASS_SPECTROMETRY: ION_MODE (.+)$")
					if ( ! is.na(g[1,1])) {
						spectrum$setField(BIODB.MSMODE, if (g[1,2] == 'POSITIVE') BIODB.MSMODE.POS else BIODB.MSMODE.NEG)
						next
					}

					# PEAKS
					if (.parse.peak.line(spectrum, s))
						next
				}
			}

			spectra <- c(spectra, spectrum)
		}

		# Replace elements with no accession id by NULL
		spectra <- lapply(spectra, function(x) if (is.na(x$getField(BIODB.ACCESSION))) NULL else x)

		# Set associated compounds
		compounds <- createMassbankCompoundFromTxt(contents)
		for (i in seq(spectra))
			if ( ! is.null(spectra[[i]])) {
				spectra[[i]]$setField(BIODB.COMPOUND, compounds[[i]])
			}

		# If the input was a single element, then output a single object
		if (drop && length(contents) == 1)
			spectra <- spectra[[1]]

		return(spectra)
	}

	###################
	# PARSE PEAK LINE #
	###################

	.parse.peak.line <- function(spectrum, line) {

		peaks <- BIODB.PEAK.DF.EXAMPLE
		
		# Annotation
		g <- str_match(line, "^\\s+([0-9][0-9.]*) ([A-Z0-9+-]+) ([0-9]+) ([0-9][0-9.]*) ([0-9][0-9.]*)$")
		if ( ! is.na(g[1,1]))
			peaks[1, c(BIODB.PEAK.MZ, BIODB.PEAK.FORMULA, BIODB.PEAK.FORMULA.COUNT, BIODB.PEAK.MASS, BIODB.PEAK.ERROR.PPM)] <- list(as.double(g[1,2]), g[1,3], as.integer(g[1,4]), as.double(g[1,5]), as.double(g[1,6]))

		# Peak
		g <- str_match(line, "^\\s+([0-9][0-9.]*) ([0-9][0-9.]*) ([0-9]+)$")
		if ( ! is.na(g[1,1]))
			peaks[1, c(BIODB.PEAK.MZ, BIODB.PEAK.INTENSITY, BIODB.PEAK.RELATIVE.INTENSITY)] <- list(as.double(g[1,2]), as.double(g[1,3]), as.integer(g[1,4]))

		if (nrow(peaks) > 0) {

			# Get curent peaks and merge with new peaks
			current.peaks <- spectrum$getField(BIODB.PEAKS)
			if ( ! is.null(current.peaks))
				peaks <- rbind(current.peaks, peaks)

			spectrum$setField(BIODB.PEAKS, peaks)

			return(TRUE)
		}

		return(FALSE)
	}
}
