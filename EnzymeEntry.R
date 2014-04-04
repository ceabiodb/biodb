source('TxtEntry.R', chdir = TRUE)

#####################
# CLASS DECLARATION #
#####################

EnzymeEntry <- setRefClass("EnzymeEntry", contains = 'TxtEntry', fields = list(desc = "character"))

###############
# CONSTRUCTOR #
###############

EnzymeEntry$methods(
	initialize = function(desc = NA_character_, ...) {
		desc <<- desc
		callSuper(...) # calls super-class initializer with remaining parameters
	}
)

###############
# DESCRIPTION #
###############

EnzymeEntry$methods(
	getDescription = function() {
		return(.self$desc)
	}
)

###########
# KEGG ID #
###########

EnzymeEntry$methods(
	getKeggId = function() {
		return(paste('ec', .self$getId(), sep=":"))
	}
)

###########
# FACTORY #
###########

createEnzymeEntryFromText <- function(text) {
	lines <- strsplit(text, "\n")
	id <- NA_character_
	desc <- NA_character_
	for (s in lines[[1]]) {

		# ID
		g <- str_match(s, "^ID\\s+([0-9.]+)$")
		if ( ! is.na(g[1,1]))
			id <- g[1,2]

		# Description
		g <- str_match(s, "^DE\\s+(.+)$")
		if ( ! is.na(g[1,1]))
			desc <- g[1,2]
	}
	if (is.na(id)) return(NULL)
	entry <- EnzymeEntry$new(id = id, desc = desc, .orig_text_file = text)
	return(entry)
}
