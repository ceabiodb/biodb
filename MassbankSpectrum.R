if ( ! exists('MassbankSpectrum')) { # Do not load again if already loaded

	source('BiodbEntry.R')
	
	#####################
	# CLASS DECLARATION #
	#####################
	
	MassbankSpectrum <- setRefClass("MassbankSpectrum", contains = "BiodbEntry")

	###########
	# FACTORY #
	###########
	
	createMassbankSpectrumFromTxt <- function(text) {

		library(stringr)

		# Create instance
		spectrum <- MassbankSpectrum$new()

		# Read text
		lines <- strsplit(text, "\n")
		for (s in lines[[1]]) {
	
			# ID
			g <- str_match(s, "ACCESSION: (.+)$")
			if ( ! is.na(g[1,1]))
				compound$setField(RBIODB.ACCESSION, g[1,2])
		}

		return(if (is.null(compound$getField(RBIODB.ACCESSION))) NULL else compound)
	}
}
