source('BiodbEntry.R')
source('../r-lib/hshhlp.R', chdir = TRUE)
source('../r-lib/strhlp.R', chdir = TRUE)

#####################
# CLASS DECLARATION #
#####################

LipidmapsEntry <- setRefClass("LipidmapsEntry", contains = 'BiodbEntry', fields = list(hmdb_id = 'character', synonyms = 'character', formula = 'character', mass = 'numeric'))

###############
# CONSTRUCTOR #
###############

LipidmapsEntry$methods(
	initialize = function(hmdb_id = NA_character_, synonyms = NA_character_, formula = NA_character_, mass = NA_real_, ...) {
		hmdb_id <<- if ( ! is.null(hmdb_id)) hmdb_id else NA_character_
		synonyms <<- if ( ! is.null(synonyms)) synonyms else NA_character_
		formula <<- if ( ! is.null(formula)) formula else NA_character_
		mass <<- if ( ! is.null(mass)) mass else NA_real_
		callSuper(...) # calls super-class initializer with remaining parameters
	}
)

###########
# HMDB ID #
###########

LipidmapsEntry$methods(
	getHmdbId = function() {
		return(.self$hmdb_id)
	}
)

############
# SYNONYMS #
############

LipidmapsEntry$methods(
	getSynonyms = function() {
		return(.self$synonyms)
	}
)

###########
# FORMULA #
###########

LipidmapsEntry$methods(
	getFormula = function() {
		return(.self$formula)
	}
)

########
# MASS #
########

LipidmapsEntry$methods(
	getMass = function() {
		return(.self$mass)
	}
)

###########
# FACTORY #
###########

createLipidmapsEntryFromCsv <- function(text) {

	# Split test in lines
	lines <- strsplit(text, "\n")
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

	return(if (is.na(id)) NULL else LipidmapsEntry$new(id = id, kegg_id = kegg_id, hmdb_id = hmdb_id, mass = mass, formula = formula, synonyms = synonyms))
}
