if ( ! exists('MassbankCompound')) { # Do not load again if already loaded

	source('BiodbEntry.R')
	
	#####################
	# CLASS DECLARATION #
	#####################
	
	MassbankCompound <- setRefClass("MassbankCompound", contains = "BiodbEntry")

	###########
	# FACTORY #
	###########
	
	createMassbankCompoundFromTxt <- function(text) {

		library(stringr)

		# Create instance
		compound <- MassbankCompound$new()

		# Read text
		lines <- strsplit(text, "\n")
		for (s in lines[[1]]) {

			# NAME
			if (is.na(name)) {
				g <- str_match(s, "^CH\\$NAME:\\s+(.+)$")
				if ( ! is.na(g[1,1]))
					compound$setField(RBIODB.NAME, g[1,2])
			}
	
			# CHEBI ID
			g <- str_match(s, "^CH\\$LINK: CHEBI\\s+(.+)$")
			if ( ! is.na(g[1,1]))
				compound$setField(RBIODB.CHEBI.ID, g[1,2])
	
			# KEGG ID
			g <- str_match(s, "^CH\\$LINK: KEGG\\s+(.+)$")
			if ( ! is.na(g[1,1]))
				compound$setField(RBIODB.KEGG.ID, g[1,2])
	
			# PUBCHEM ID
			g <- str_match(s, "^CH\\$LINK: PUBCHEM\\s+(.+)$")
			if ( ! is.na(g[1,1]))
				compound$setField(RBIODB.PUBCHEM.ID, g[1,2])
	
			# INCHI
			g <- str_match(s, "^CH\\$IUPAC:\\s+(.+)$")
			if ( ! is.na(g[1,1]))
				compound$setField(RBIODB.INCHI, g[1,2])
		}

		return(if (is.null(compound$getField(RBIODB.NAME))) NULL else compound)
	}
}
