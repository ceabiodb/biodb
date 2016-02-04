library(stringr)
source('BiodbCompound.R')

#####################
# CLASS DECLARATION #
#####################

EnzymeCompound <- setRefClass("EnzymeCompound", contains = 'BiodbCompound', fields = list(desc = "character"))

###############
# CONSTRUCTOR #
###############

EnzymeCompound$methods(
	initialize = function(desc = NA_character_, ...) {
		desc <<- if ( ! is.null(desc)) desc else NA_character_
		callSuper(...) # calls super-class initializer with remaining parameters
	}
)

###############
# DESCRIPTION #
###############

EnzymeCompound$methods(
	getDescription = function() {
		return(.self$desc)
	}
)

###########
# FACTORY #
###########

createEnzymeCompoundFromText <- function(text) {
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

	return(if (is.na(id)) NULL else EnzymeCompound$new(id = id, kegg_id = paste('ec', id, sep=":"), desc = desc))
}
