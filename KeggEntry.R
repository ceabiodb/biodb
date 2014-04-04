library("stringr")
source('Entry.R', chdir = TRUE)

#####################
# CLASS DECLARATION #
#####################

KeggEntry <- setRefClass("KeggEntry", contains = 'Entry', fields = list(id = "character", lipidmaps_id = "character", .orig_text_file = "character" ))

###############
# CONSTRUCTOR #
###############

KeggEntry$methods(
	initialize = function(id = NA_character_, lipidmaps_id = NA_character_, .orig_text_file = "", ...) {
		id <<- id
		lipidmaps_id <<- lipidmaps_id
		callSuper(...) # calls super-class initializer with remaining parameters
	}
)

######
# ID #
######

KeggEntry$methods(
	getId = function() {
		return(.self$id)
	}
)

################
# LIPIDMAPS ID #
################

KeggEntry$methods(
	getLipidmapsId = function() {
		return(.self$lipidmaps_id)
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
	id <- NA_character_
	organism <- NA_character_
	lipidmapsid <- NA_character_
	for (s in lines[[1]]) {

		# ENZYME ID
		g <- str_match(s, "^ENTRY\\s+EC\\s+(\\S+)")
		if ( ! is.na(g[1,1]))
			id <- paste('ec', g[1,2], sep = ':')

		# COMPOUND ID
		else {
			g <- str_match(s, "^ENTRY\\s+(\\S+)\\s+Compound")
			if ( ! is.na(g[1,1]))
				id <- paste('cpd', g[1,2], sep = ':')

			# OTHER ID
			else {
				g <- str_match(s, "^ENTRY\\s+(\\S+)")
				if ( ! is.na(g[1,1]))
					id <- g[1,2]
			}
		}

		# ORGANISM
		g <- str_match(s, "^ORGANISM\\s+(\\S+)")
		if ( ! is.na(g[1,1]))
			id <- paste(g[1,2], id, sep = ':')

		# LIPIDMAPS
		g <- str_match(s, "^\\s+LIPIDMAPS:\\s+(\\S+)")
		if ( ! is.na(g[1,1]))
			lipidmapsid <- g[1,2]
	}
	if (is.na(id)) return(NULL)
	entry <- KeggEntry$new(id = id, lipidmaps_id = lipidmapsid)
	entry$.setOrigTextFile(text)
	return(entry)
}

