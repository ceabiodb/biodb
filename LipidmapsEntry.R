source('TxtEntry.R', chdir = TRUE)

#####################
# CLASS DECLARATION #
#####################

LipidmapsEntry <- setRefClass("LipidmapsEntry", contains = 'TxtEntry', fields = list())

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
	lines <- strsplit(text, "\n")
	id <- NA_character_
	values <- strsplit(lines[[1]][[2]], ',')
	names(values) <- strsplit(lines[[1]][[1]], ',')
	print(values)
#	desc <- NA
#	for (s in lines[[1]]) {
#
#		# ID
#		g <- str_match(s, "^ID\\s+([0-9.]+)$")
#		if ( ! is.na(g[1,1]))
#			id <- g[1,2]
#
#		# Description
#		g <- str_match(s, "^DE\\s+(.+)$")
#		if ( ! is.na(g[1,1]))
#			desc <- g[1,2]
#	}

	if (is.na(id)) return(NULL)
	entry <- LipidmapsEntry$new(id = id, .orig_text_file = text)
	return(entry)
}

