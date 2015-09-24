source('BioDbEntry.R')

#####################
# CLASS DECLARATION #
#####################

KeggEntry <- setRefClass("KeggEntry", contains = 'BioDbEntry', fields = list(lipidmaps_id = "character"))

###############
# CONSTRUCTOR #
###############

KeggEntry$methods(
	initialize = function(lipidmaps_id = NA_character_, ...) {
		lipidmaps_id <<- if ( ! is.null(lipidmaps_id)) lipidmaps_id else NA_character_
		callSuper(...) # calls super-class initializer with remaining parameters
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

###########
# FACTORY #
###########

createKeggEntryFromText <- function(text) {

	library(stringr)

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

	return(if (is.na(id)) NULL else KeggEntry$new(id = id, kegg_id = id, lipidmaps_id = lipidmapsid))
}

