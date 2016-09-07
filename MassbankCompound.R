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

		# Fields
		fields <- list()
		fields[[BIODB.CHEBI.ID]] <- "^CH\\$LINK: CHEBI\\s+(.+)$"
		fields[[BIODB.KEGG.ID]] <- "^CH\\$LINK: KEGG\\s+(.+)$"
		fields[[BIODB.PUBCHEM.ID]] <- "^CH\\$LINK: PUBCHEM\\s+(.+)$"
		fields[[BIODB.INCHI]] <- "^CH\\$IUPAC:\\s+(.+)$"
		fields[[BIODB.INCHIKEY]] <- "^CH\\$LINK: INCHIKEY\\s+(.+)$"
		fields[[BIODB.CHEMSPIDER.ID]] <- "^CH\\$LINK: CHEMSPIDER\\s+(.+)$"
		fields[[BIODB.CAS.ID]] <- "^CH\\$LINK: CAS\\s+(.+)$"
		fields[[BIODB.FORMULA]] <- "^CH\\$FORMULA:\\s+(.+)$"
		fields[[BIODB.SMILES]] <- "^CH\\$SMILES:\\s+(.+)$"
		fields[[BIODB.MASS]] <- "^CH\\$EXACT_MASS:\\s+(.+)$"

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
		
				# Other fields
				for (f in fields) {
					g <- str_match(s, fields[[f]])
					if ( ! is.na(g[1,1]))
						compound$setField(f, g[1,2])
				}
			}

			compounds <- c(compounds, compound)
		}

		# Replace elements with no accession id by NULL
		compounds <- lapply(compounds, function(x) if (is.na(x$getField(BIODB.NAME))) NULL else x)

		return(compounds)
	}
}
