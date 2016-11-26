if ( ! exists('LipidmapsEntry')) { # Do not load again if already loaded

	#####################
	# CLASS DECLARATION #
	#####################

	LipidmapsEntry <- setRefClass("LipidmapsEntry", contains = 'BiodbEntry')

	###########
	# FACTORY #
	###########

	createLipidmapsEntryFromCsv <- function(contents, drop = TRUE) {

		entries <- list()

		# Mapping column names
		col2field <- list()
		col2field[[BIODB.NAME]] <- 'COMMON_NAME'
		col2field[[BIODB.ACCESSION]] <- 'LM_ID'
		col2field[[BIODB.KEGG.ID]] <- 'KEGG_ID'
		col2field[[BIODB.HMDB.ID]] <- 'HMDBID'
		col2field[[BIODB.MASS]] <- 'MASS'
		col2field[[BIODB.FORMULA]] <- 'FORMULA'
		
		for (text in contents) {

			# Create instance
			entry <- LipidmapsEntry$new()

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
						entry$setField(field, values[[col2field[[field]]]])

				# Set names
				if (values[['SYNONYMS']] != '-') {
					# TODO
				}
			}

			entries <- c(entries, entry)
		}

		# Replace elements with no accession id by NULL
		entries <- lapply(entries, function(x) if (is.na(x$getField(BIODB.ACCESSION))) NULL else x)

		# If the input was a single element, then output a single object
		if (drop && length(contents) == 1)
			entries <- entries[[1]]
	
		return(entries)
	}
}
