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
		fields[[BIODB.INCHI]] <- "^CH\\$IUPAC:\\s+(.+)$"
		fields[[BIODB.INCHIKEY]] <- "^CH\\$LINK: INCHIKEY\\s+(.+)$"
		fields[[BIODB.CHEMSPIDER.ID]] <- "^CH\\$LINK: CHEMSPIDER\\s+(.+)$"
		fields[[BIODB.CAS.ID]] <- "^CH\\$LINK: CAS\\s+(.+)$"
		fields[[BIODB.FORMULA]] <- "^CH\\$FORMULA:\\s+(.+)$"
		fields[[BIODB.SMILES]] <- "^CH\\$SMILES:\\s+(.+)$"
		fields[[BIODB.MASS]] <- "^CH\\$EXACT_MASS:\\s+(.+)$"
		fields[[BIODB.PUBCHEMCOMP.ID]] <- "^CH\\$LINK: PUBCHEM\\s+.*CID:([0-9]+)"
		fields[[BIODB.PUBCHEMSUB.ID]] <- "^CH\\$LINK: PUBCHEM\\s+.*SID:([0-9]+)"

		compounds <- list()

		for (text in contents) {

			# Create instance
			compound <- MassbankCompound$new()

			# Read text
			lines <- strsplit(text, "\n")
			for (s in lines[[1]]) {

				# Name
				if (is.na(compound$getField(BIODB.NAME))) {
					g <- str_match(s, "^CH\\$NAME:\\s+(.+)$")
					if ( ! is.na(g[1,1]))
						compound$setField(BIODB.NAME, g[1,2])
				}
		
				# PubChem
				g <- str_match(s, "^CH\\$LINK: PUBCHEM\\s+([0-9]+)$")
				if ( ! is.na(g[1,1]))
					compound$setField(BIODB.PUBCHEMSUB.ID, g[1,2])

				# Other fields
				for (f in names(fields)) {
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
