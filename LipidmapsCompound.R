if ( ! exists('LipidmapsCompound')) { # Do not load again if already loaded

	source('BiodbEntry.R')
	source('../r-lib/strhlp.R', chdir = TRUE)

	#####################
	# CLASS DECLARATION #
	#####################

	LipidmapsCompound <- setRefClass("LipidmapsCompound", contains = 'BiodbEntry')

	###########
	# FACTORY #
	###########

	createLipidmapsCompoundFromCsv <- function(contents, drop = TRUE) {

		compounds <- list()

		# Mapping column names
		col2field <- list()
		col2field[[RBIODB.NAME]] <- 'COMMON_NAME'
		col2field[[RBIODB.ACCESSION]] <- 'LM_ID'
		col2field[[RBIODB.KEGG.ID]] <- 'KEGG_ID'
		col2field[[RBIODB.HMDB.ID]] <- 'HMDBID'
		col2field[[RBIODB.MASS]] <- 'MASS'
		col2field[[RBIODB.FORMULA]] <- 'FORMULA'
		
		for (text in contents) {

			# Create instance
			compound <- LipidmapsCompound$new()

			# Split text in lines
			lines <- split.str(text, sep = "\n", unlist = TRUE)

			# An error occured
			if ( ! grepl("No record found", lines[[2]])) {

				# Keys on first line
				keys <- split.str(lines[[1]], unlist = TRUE)

				# Values on second line
				values <- split.str(lines[[2]], unlist = TRUE)
				names(values) <- keys[seq(values)]

				# Get field values
				for (field in names(col2field))
					if (values[[col2field[[field]]]] != '-')
						compound$setField(field, values[[col2field[[field]]]])

				# Set names
				if (values[['SYNONYMS']] != '-') {
					# TODO
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
