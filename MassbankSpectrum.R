if ( ! exists('MassbankSpectrum')) { # Do not load again if already loaded

	source('BiodbEntry.R')
	
	#####################
	# CLASS DECLARATION #
	#####################
	
	MassbankSpectrum <- setRefClass("MassbankSpectrum", contains = "BiodbEntry")

	###########
	# FACTORY #
	###########
	
	createMassbankSpectrumFromTxt <- function(contents) {

		library(stringr)

		spectra <- list()

		for (text in contents) {

			# Create instance
			spectrum <- MassbankSpectrum$new()

			# Read text
			lines <- strsplit(text, "\n")
			for (s in lines[[1]]) {
		
				# ID
				g <- str_match(s, "ACCESSION: (.+)$")
				if ( ! is.na(g[1,1]))
					spectrum$setField(RBIODB.ACCESSION, g[1,2])
			}

			spectra <- c(spectra, spectrum)
		}

		# Replace elements with no accession id by NULL
		spectra <- lapply(spectra, function(x) if (is.na(x$getField(RBIODB.ACCESSION))) NULL else x)

		return(spectra)
	}
}
