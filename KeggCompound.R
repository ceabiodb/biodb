if ( ! exists('KeggCompound')) { # Do not load again if already loaded

	source('BiodbCompound.R')
	
	#####################
	# CLASS DECLARATION #
	#####################
	
	KeggCompound <- setRefClass("KeggCompound", contains = 'BiodbCompound', fields = list(.lipidmapsid = "character", .chebiid = "character", .name = "character"))
	
	###############
	# CONSTRUCTOR #
	###############
	
	KeggCompound$methods( initialize = function(lipidmapsid = NA_character_, chebiid = NA_character_, name = NA_character_, ...) {
	
			.lipidmapsid <<- if ( ! is.null(lipidmapsid)) lipidmapsid else NA_character_
			.chebiid <<- if ( ! is.null(chebiid)) chebiid else NA_character_
			.name <<- if ( ! is.null(name)) name else NA_character_
	
			callSuper(...) # calls super-class initializer with remaining parameters
		}
	)
	
	###########
	# KEGG ID #
	###########
	
	KeggCompound$methods(	getKeggId = function() {
		return(.self$getId())
	})
	
	########
	# NAME #
	########
	
	KeggCompound$methods( getName = function() {
		return(.self$.name)
	})
	
	############
	# CHEBI ID #
	############
	
	KeggCompound$methods( getChebiId = function() {
		return(.self$.chebiid)
	})
	
	############
	# INCHIKEY #
	############
	
	KeggCompound$methods( getInchiKey = function() {
	
		inchi <- NA_character_
	
		if ( ! is.null(.self$.factory)) {
			chebi.compound <- .self$.factory$createCompoundFromDb(RBIODB.CHEBI, .self$getChebiId())
			if ( ! is.null(chebi.compound))
				inchi <- chebi.compound$getInchiKey()
		}
	
		return(inchi)
	})
	
	#########
	# INCHI #
	#########
	
	KeggCompound$methods( getInchi = function() {
	
		inchi <- NA_character_
	
		if ( ! is.null(.self$.factory)) {
			chebi.compound <- .self$.factory$createCompoundFromDb(RBIODB.CHEBI, .self$getChebiId())
			if ( ! is.null(chebi.compound))
				inchi <- chebi.compound$getInchi()
		}
	
		return(inchi)
	})
	
	################
	# LIPIDMAPS ID #
	################
	
	KeggCompound$methods( getLipidmapsId = function() {
		return(.self$.lipidmapsid)
	})
	
	###########
	# FACTORY #
	###########
	
	createKeggCompoundFromText <- function(text) {
	
		library(stringr)
	
		lines <- strsplit(text, "\n")
		id <- NA_character_
		organism <- NA_character_
		lipidmapsid <- NA_character_
		chebiid <- NA_character_
		name <- NA_character_
		for (s in lines[[1]]) {
	
			# ENZYME ID
			g <- str_match(s, "^ENTRY\\s+EC\\s+(\\S+)")
			if ( ! is.na(g[1,1]))
				id <- paste('ec', g[1,2], sep = ':')
	
			# ENTRY ID
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
	
			# NAME
			g <- str_match(s, "^NAME\\s+([^,;\\s]+)")
			if ( ! is.na(g[1,1]))
				name <- g[1,2]
	
			# CHEBI
			g <- str_match(s, "^\\s+ChEBI:\\s+(\\S+)")
			if ( ! is.na(g[1,1]))
				chebiid <- g[1,2]
	
			# LIPIDMAPS
			g <- str_match(s, "^\\s+LIPIDMAPS:\\s+(\\S+)")
			if ( ! is.na(g[1,1]))
				lipidmapsid <- g[1,2]
		}
	
		return(if (is.na(id)) NULL else KeggCompound$new(id = id, lipidmapsid = lipidmapsid, chebiid = chebiid, name = name))
	}
}	
