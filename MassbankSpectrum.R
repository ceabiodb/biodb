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
		regex[[RBIODB.ACCESSION]] <- "^ACCESSION: (.+)$"
		regex[[RBIODB.MSDEV]] <- "^AC\\$INSTRUMENT: (.+)$"
		regex[[RBIODB.MSDEVTYPE]] <- "^AC\\$INSTRUMENT_TYPE: (.+)$"
		regex[[RBIODB.MSTYPE]] <- "^AC\\$MASS_SPECTROMETRY: MS_TYPE (.+)$"
		regex[[RBIODB.MSPRECMZ]] <- "^MS\\$FOCUSED_ION: PRECURSOR_M/Z (.+)$"
		regex[[RBIODB.NB.PEAKS]] <- "^PK\\$NUM_PEAK: ([0-9]+)$"
		regex[[RBIODB.MSPRECANNOT]] <- "^MS\\$FOCUSED_ION: PRECURSOR_TYPE (.+)$"

		for (text in contents) {

			# Create instance
			spectrum <- MassbankSpectrum$new()

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
					spectrum$setField(RBIODB.MSMODE, if (g[1,2] == 'POSITIVE') RBIODB.MSMODE.POS else RBIODB.MSMODE.NEG)
					next
				}

				# PEAKS
				if (.parse.peak.line(spectrum, s))
					next
			}

			spectra <- c(spectra, spectrum)
		}

		# Replace elements with no accession id by NULL
		spectra <- lapply(spectra, function(x) if (is.na(x$getField(RBIODB.ACCESSION))) NULL else x)

		# Set associated compounds
		compounds <- createMassbankCompoundFromTxt(contents)
		for (i in seq(spectra))
			if ( ! is.null(spectra[[i]]))
				spectra[[i]]$setField(RBIODB.COMPOUND, compounds[[i]])

		# If the input was a single element, then output a single object
		if (drop && length(contents) == 1)
			spectra <- spectra[[1]]

		return(spectra)
	}

	###################
	# PARSE PEAK LINE #
	###################

	.parse.peak.line <- function(spectrum, line) {

		peaks <- RBIODB.PEAK.DF.EXAMPLE
		
		# Annotation
		g <- str_match(line, "^\\s+([0-9][0-9.]*) ([A-Z0-9+-]+) ([0-9]+) ([0-9][0-9.]*) ([0-9][0-9.]*)$")
		if ( ! is.na(g[1,1]))
			peaks[1, c(RBIODB.PEAK.MZ, RBIODB.PEAK.FORMULA, RBIODB.PEAK.FORMULA.COUNT, RBIODB.PEAK.MASS, RBIODB.PEAK.ERROR.PPM)] <- list(as.double(g[1,2]), g[1,3], as.integer(g[1,4]), as.double(g[1,5]), as.double(g[1,6]))

		# Peak
		g <- str_match(line, "^\\s+([0-9][0-9.]*) ([0-9][0-9.]*) ([0-9]+)$")
		if ( ! is.na(g[1,1]))
			peaks[1, c(RBIODB.PEAK.MZ, RBIODB.PEAK.INTENSITY, RBIODB.PEAK.RELATIVE.INTENSITY)] <- list(as.double(g[1,2]), as.double(g[1,3]), as.integer(g[1,4]))

		if (nrow(peaks) > 0) {

			# Get curent peaks and merge with new peaks
			current.peaks <- spectrum$getField(RBIODB.PEAKS)
			if ( ! is.null(current.peaks))
				peaks <- rbind(current.peaks, peaks)

			spectrum$setField(RBIODB.PEAKS, peaks)

			return(TRUE)
		}

		return(FALSE)
	}
}
