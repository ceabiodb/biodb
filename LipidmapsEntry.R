source('BioDbEntry.R')
source('../r-lib/hshhlp.R', chdir = TRUE)

#####################
# CLASS DECLARATION #
#####################

LipidmapsEntry <- setRefClass("LipidmapsEntry", contains = 'BioDbEntry', fields = list())

###############
# CONSTRUCTOR #
###############

LipidmapsEntry$methods(
	initialize = function(...) {
		callSuper(...) # calls super-class initializer with remaining parameters
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
	id <- if (hHasKey(values, 'LM_ID')) values[['LM_ID']] else NA_character_
	kegg_id <- if (hHasKey(values, 'KEGG_ID')) values[['KEGG_ID']] else NA_character_

	return(if (is.na(id)) NULL else LipidmapsEntry$new(id = id, kegg_id = kegg_id))
}
