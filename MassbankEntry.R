if ( ! exists('MassbankEntry')) { # Do not load again if already loaded

	source('BiodbEntry.R')
	
	#####################
	# CLASS DECLARATION #
	#####################
	
	MassbankEntry <- setRefClass("MassbankEntry", contains = "BiodbEntry", fields = list(.inchi = "character", .name = "character", .chebiid = "character", .keggid = "character", .pubchemid = "character"))
	
	###############
	# CONSTRUCTOR #
	###############
	
	MassbankEntry$methods( initialize = function(id = NA_character_, inchi = NA_character_, name = NA_character_, chebiid = NA_character_, pubchemid = NA_character_, keggid = NA_character_, ...) {
	
		.inchi <<- if ( ! is.null(inchi)) inchi else NA_character_
		.name <<- if ( ! is.null(name)) name else NA_character_
		.chebiid <<- if ( ! is.null(chebiid)) chebiid else NA_character_
		.pubchemid <<- if ( ! is.null(pubchemid)) pubchemid else NA_character_
		.keggid <<- if ( ! is.null(keggid)) keggid else NA_character_
	
		callSuper(id = id, ...)
	})
	
	#########
	# INCHI #
	#########
	
	MassbankEntry$methods(	getInchi = function() {
		return(.self$.inchi)
	})
	
	########
	# NAME #
	########
	
	MassbankEntry$methods( getName = function() {
		return(.self$.name)
	})
	
	############
	# CHEBI ID #
	############
	
	MassbankEntry$methods( getChebiId = function() {
		return(.self$.chebiid)
	})
	
	###########
	# KEGG ID #
	###########
	
	MassbankEntry$methods( getKeggId = function() {
		return(.self$.keggid)
	})
	
	##############
	# PUBCHEM ID #
	##############
	
	MassbankEntry$methods( getPubchemId = function() {
		return(.self$.pubchemid)
	})

	###########
	# FACTORY #
	###########
	
	createMassbankEntryFromTxt <- function(text) {

		library(stringr)
	
		id <- NA_character_
		inchi <- NA_character_
		chebiid <- NA_character_
		keggid <- NA_character_
		pubchemid <- NA_character_
		name <- NA_character_

		# Read text
		lines <- strsplit(text, "\n")
		for (s in lines[[1]]) {
	
			# ID
			g <- str_match(s, "ACCESSION: (.+)$")
			if ( ! is.na(g[1,1]))
				id <- g[1,2]

			# NAME
			if (is.na(name)) {
				g <- str_match(s, "^CH\\$NAME:\\s+(.+)$")
				if ( ! is.na(g[1,1]))
					name <- g[1,2]
			}
	
			# CHEBI ID
			g <- str_match(s, "^CH\\$LINK: CHEBI\\s+(.+)$")
			if ( ! is.na(g[1,1]))
				chebiid <- g[1,2]
	
			# KEGG ID
			g <- str_match(s, "^CH\\$LINK: KEGG\\s+(.+)$")
			if ( ! is.na(g[1,1]))
				keggid <- g[1,2]
	
			# PUBCHEM ID
			g <- str_match(s, "^CH\\$LINK: PUBCHEM\\s+(.+)$")
			if ( ! is.na(g[1,1]))
				pubchemid <- g[1,2]
	
			# INCHI
			g <- str_match(s, "^CH\\$IUPAC:\\s+(.+)$")
			if ( ! is.na(g[1,1]))
				inchi <- g[1,2]
		}

		# Create entry
		entry <- if (is.na(id)) NULL else MassbankEntry$new(id = id, inchi = inchi, name = name, chebiid = chebiid, keggid = keggid, pubchemid = pubchemid)
	
		return(entry)
	}
}
