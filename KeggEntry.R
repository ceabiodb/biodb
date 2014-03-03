library("stringr")
source('Entry.R', chdir = TRUE)

#####################
# CLASS DECLARATION #
#####################

KeggEntry <- setRefClass("KeggEntry", contains = 'Entry', fields = list(id = "character", organism = "character", .orig_text_file = "character" ))

###############
# CONSTRUCTOR #
###############

KeggEntry$methods(
	initialize = function(id = NA_character_, organism = NA_character_, .orig_text_file = "", ...) {
		id <<- id
		organism <<- organism
		callSuper(...) # calls super-class initializer with remaining parameters
	}
)

######
# ID #
######

KeggEntry$methods(
	getId = function() {
		s <- .self$id
		if ( ! is.na(.self$organism))
			s <- paste(.self$organism, s, sep=':')
		return(s)
	}
)

######################
# ORIGINAL TEXT FILE #
######################

KeggEntry$methods(
	.setOrigTextFile = function(text) {
		.orig_text_file <<- text
	}
)

########
# SAVE #
########

KeggEntry$methods(
	save = function(filename) {
		fileConn<-file(filename)
		writeLines(.self$.orig_text_file, fileConn)
		close(fileConn)
	}
)


###########
# FACTORY #
###########

createKeggEntryFromText <- function(text) {
	lines <- strsplit(text, "\n")
	id <- NA
	organism <- NA
	for (s in lines[[1]]) {

		# ID
		g <- str_match(s, "^ENTRY\\s+(\\S+)")
		if ( ! is.na(g[1,1]))
			id <- g[1,2]

		# ORGANISM
		g <- str_match(s, "^ORGANISM\\s+(\\S+)")
		if ( ! is.na(g[1,1]))
			organism <- g[1,2]
	}
	if (is.na(id)) return(NULL)
	entry <- KeggEntry$new(id = id, organism = organism)
	entry$.setOrigTextFile(text)
	return(entry)
}

