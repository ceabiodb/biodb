if ( ! exists('MassbankCompound')) { # Do not load again if already loaded

	source('BiodbEntry.R')
	
	#####################
	# CLASS DECLARATION #
	#####################
	
	MassbankCompound <- setRefClass("MassbankCompound", contains = "BiodbEntry")

	###########
	# FACTORY #
	###########
	
	createMassbankCompoundFromTxt <- function(contents) {

		library(stringr)

		compounds <- list()

		for (text in contents) {

			# Create instance
			compound <- MassbankCompound$new()

			# Read text
			lines <- strsplit(text, "\n")
			for (s in lines[[1]]) {

				# NAME
				if (is.na(compound$getField(BIODB.NAME))) {
					g <- str_match(s, "^CH\\$NAME:\\s+(.+)$")
					if ( ! is.na(g[1,1]))
						compound$setField(BIODB.NAME, g[1,2])
				}
		
				# CHEBI ID
				g <- str_match(s, "^CH\\$LINK: CHEBI\\s+(.+)$")
				if ( ! is.na(g[1,1]))
					compound$setField(BIODB.CHEBI.ID, g[1,2])
		
				# KEGG ID
				g <- str_match(s, "^CH\\$LINK: KEGG\\s+(.+)$")
				if ( ! is.na(g[1,1]))
					compound$setField(BIODB.KEGG.ID, g[1,2])
		
				# PUBCHEM ID
				g <- str_match(s, "^CH\\$LINK: PUBCHEM\\s+(.+)$")
				if ( ! is.na(g[1,1]))
					compound$setField(BIODB.PUBCHEM.ID, g[1,2])
		
				# INCHI
				g <- str_match(s, "^CH\\$IUPAC:\\s+(.+)$")
				if ( ! is.na(g[1,1]))
					compound$setField(BIODB.INCHI, g[1,2])
			}

			compounds <- c(compounds, compound)
		}

		# Replace elements with no accession id by NULL
		compounds <- lapply(compounds, function(x) if (is.na(x$getField(BIODB.NAME))) NULL else x)

		return(compounds)
	}
}
