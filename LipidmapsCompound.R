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

		# Split text in lines
		lines <- strsplit(text, "\n")[[1]]

		# keys on first line
		# values on second line

		value_line <- lines[[1]][[2]]

		# An error occured
		if (grepl("No record found", value_line))
			return(NULL)

		# Get keys (on first line) and values (on second line)
		keys <- unlist(strsplit(lines[[1]][[1]], ','))
		values <- unlist(strsplit(value_line, ','))
		values <- hCreate(keys, values)

		# Extract data
		id <- if (hHasKey(values, 'LM_ID') && values[['LM_ID']] != '-') values[['LM_ID']] else NA_character_
		kegg_id <- if (hHasKey(values, 'KEGG_ID') && values[['KEGG_ID']] != '-') values[['KEGG_ID']] else NA_character_
		hmdb_id <- if (hHasKey(values, 'HMDBID') && values[['HMDBID']] != '-') values[['HMDBID']] else NA_character_
		mass <- if (hHasKey(values, 'MASS') && values[['MASS']] != '-') as.numeric(values[['MASS']]) else NA_real_
		formula <- if (hHasKey(values, 'FORMULA') && values[['FORMULA']] != '-') values[['FORMULA']] else NA_character_
		synonyms <- if (hHasKey(values, 'SYNONYMS') && values[['SYNONYMS']] != '-') split(values[['SYNONYMS']], sep = ';', unlist = TRUE) else NA_character_

		return(if (is.na(id)) NULL else LipidmapsCompound$new(id = id, kegg_id = kegg_id, hmdb_id = hmdb_id, mass = mass, formula = formula, synonyms = synonyms))
	}
}
