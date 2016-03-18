if ( ! exists('EnzymeCompound')) { # Do not load again if already loaded

	source('BiodbEntry.R')

	#####################
	# CLASS DECLARATION #
	#####################

	EnzymeCompound <- setRefClass("EnzymeCompound", contains = 'BiodbEntry')

	###########
	# FACTORY #
	###########

	createEnzymeCompoundFromTxt <- function(contents, drop = TRUE) {

		library(stringr)

		compounds <- list()
	
		# Define fields regex
		regex <- character()
		regex[[RBIODB.ACCESSION]] <- "^ID\\s+([0-9.]+)$"
		regex[[RBIODB.DESCRIPTION]] <- "^DE\\s+(.+)$"

		for (text in contents) {

			# Create instance
			compound <- EnzymeCompound$new()

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
