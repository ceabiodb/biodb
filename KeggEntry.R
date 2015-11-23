source('BiodbEntry.R')

#####################
# CLASS DECLARATION #
#####################

KeggEntry <- setRefClass("KeggEntry", contains = 'BiodbEntry', fields = list(.lipidmapsid = "character", .chebiid = "character"))

###############
# CONSTRUCTOR #
###############

KeggEntry$methods( initialize = function(lipidmapsid = NA_character_, chebiid = NA_character_, ...) {

		.lipidmapsid <<- if ( ! is.null(lipidmapsid)) lipidmapsid else NA_character_
		.chebiid <<- if ( ! is.null(chebiid)) chebiid else NA_character_

		callSuper(...) # calls super-class initializer with remaining parameters
	}
)

###########
# KEGG ID #
###########

KeggEntry$methods(	getKeggId = function() {
	return(.self$getId())
})

############
# CHEBI ID #
############

KeggEntry$methods( getChebiId = function() {
	return(.self$.chebiid)
})

#########
# INCHI #
#########

KeggEntry$methods( getInchi = function() {

	inchi <- NA_character_

	if ( ! is.null(.self$.factory)) {
		chebi.entry <- .self$.factory$createEntryFromDb(RBIODB.CHEBI, .self$getChebiId())
		if ( ! is.null(chebi.entry))
			inchi <- chebi.entry$getInchi()
	}

	return(inchi)
})

################
# LIPIDMAPS ID #
################

KeggEntry$methods( getLipidmapsId = function() {
	return(.self$.lipidmapsid)
})

###########
# FACTORY #
###########

createKeggEntryFromText <- function(text) {

	library(stringr)

	lines <- strsplit(text, "\n")
	id <- NA_character_
	organism <- NA_character_
	lipidmapsid <- NA_character_
	chebiid <- NA_character_
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

		# CHEBI
		g <- str_match(s, "^\\s+ChEBI:\\s+(\\S+)")
		if ( ! is.na(g[1,1]))
			chebiid <- g[1,2]

		# LIPIDMAPS
		g <- str_match(s, "^\\s+LIPIDMAPS:\\s+(\\S+)")
		if ( ! is.na(g[1,1]))
			lipidmapsid <- g[1,2]
	}

	return(if (is.na(id)) NULL else KeggEntry$new(id = id, lipidmapsid = lipidmapsid, chebiid = chebiid))
}

