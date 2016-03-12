if ( ! exists('KeggCompound')) { # Do not load again if already loaded

	source('BiodbEntry.R')
	
	#####################
	# CLASS DECLARATION #
	#####################
	
	KeggCompound <- setRefClass("KeggCompound", contains = 'BiodbEntry')
	
	###########
	# FACTORY #
	###########
	
	createKeggCompoundFromTxt <- function(contents, drop = TRUE) {
	
		library(stringr)

		compounds <- list()
	
		# Define fields regex
		regex <- character()
		regex[[RBIODB.NAME]] <- "^NAME\\s+([^,;]+)"
		regex[[RBIODB.CHEBI.ID]] <- "^\\s+ChEBI:\\s+(\\S+)"
		regex[[RBIODB.LIPIDMAPS.ID]] <- "^\\s+LIPIDMAPS:\\s+(\\S+)"

		for (text in contents) {

			# Create instance
			compound <- KeggCompound$new()

			lines <- strsplit(text, "\n")
			for (s in lines[[1]]) {

				# Test generic regex
				parsed <- FALSE
				for (field in names(regex)) {
					g <- str_match(s, regex[[field]])
					if ( ! is.na(g[1,1])) {
						compound$setField(field, g[1,2])
						parsed <- TRUE
						break
					}
				}
				if (parsed)
					next
	
				# ACCESSION
				{
					# ENZYME ID
					g <- str_match(s, "^ENTRY\\s+EC\\s+(\\S+)")
					if ( ! is.na(g[1,1]))
						compound$setField(RBIODB.ACCESSION, paste('ec', g[1,2], sep = ':'))

					# ENTRY ID
					else {
						g <- str_match(s, "^ENTRY\\s+(\\S+)\\s+Compound")
						if ( ! is.na(g[1,1]))
							compound$setField(RBIODB.ACCESSION, paste('cpd', g[1,2], sep = ':'))

						# OTHER ID
						else {
							g <- str_match(s, "^ENTRY\\s+(\\S+)")
							if ( ! is.na(g[1,1]))
								compound$setField(RBIODB.ACCESSION, g[1,2])
						}
					}
	
					# ORGANISM
					g <- str_match(s, "^ORGANISM\\s+(\\S+)")
					if ( ! is.na(g[1,1]))
						compound$setField(RBIODB.ACCESSION, paste(g[1,2], compound$getField(RBIODB.ACCESSION), sep = ':'))
				}
			}

			compounds <- c(compounds, compound)
		}

		# Replace elements with no accession id by NULL
		compounds <- lapply(compounds, function(x) if (is.na(x$getField(RBIODB.ACCESSION))) NULL else x)

		# If the input was a single element, then output a single object
		if (drop && length(contents) == 1)
			compounds <- compounds[[1]]
	
		return(compounds)
	}
}	
