library("stringr")

#####################
# CLASS DECLARATION #
#####################

EnzymeEntry <- setRefClass("EnzymeEntry", fields = list(id = "character", desc = "character"))

###############
# CONSTRUCTOR #
###############

EnzymeEntry$methods(
	initialize = function(id = "", desc = "", ...) {
		id <<- id
		desc <<- desc
		callSuper(...) # calls super-class initializer with remaining parameters
	}
)

######
# ID #
######

EnzymeEntry$methods(
	getId = function() {
		return(.self$id)
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
# FACTORY #
###########

createEnzymeEntryFromText <- function(text) {
	lines <- strsplit(text, "\n")
	id <- NA
	desc <- NA
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
	return(EnzymeEntry$new(id = id, desc = desc))
}
