library("stringr")
source('Entry.R', chdir = TRUE)

#####################
# CLASS DECLARATION #
#####################

EnzymeEntry <- setRefClass("EnzymeEntry", contains = 'Entry', fields = list(id = "character", desc = "character", .orig_text_file = "character" ))

###############
# CONSTRUCTOR #
###############

EnzymeEntry$methods(
	initialize = function(id = "", desc = "", .orig_text_file = "", ...) {
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

######################
# ORIGINAL TEXT FILE #
######################

EnzymeEntry$methods(
	.setOrigTextFile = function(text) {
		.orig_text_file <<- text
	}
)

########
# SAVE #
########

EnzymeEntry$methods(
	save = function(filename) {
		fileConn<-file(filename)
		writeLines(.self$.orig_text_file, fileConn)
		close(fileConn)
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
	entry <- EnzymeEntry$new(id = id, desc = desc)
	entry$.setOrigTextFile(text)
	return(entry)
}
