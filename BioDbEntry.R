########################
# Entry ABSTRACT CLASS #
########################

BioDbEntry <- setRefClass("BioDbEntry", fields = list(.id = "character", .keggid = "character", .inchi = "character", .inchikey = "character"))

###############
# CONSTRUCTOR #
###############

BioDbEntry$methods( initialize = function(id = NA_character_, keggid = NA_character_, inchi = NA_character_, inchikey = NA_character_, ...) {
	.id <<- if ( ! is.null(id)) id else NA_character_
	.keggid <<- if ( ! is.null(keggid)) keggid else NA_character_
	.inchi <<- if ( ! is.null(inchi)) inchi else NA_character_
	.inchikey <<- if ( ! is.null(inchikey)) inchikey else NA_character_
	callSuper(...) # calls super-class initializer with remaining parameters
})

######
# ID #
######

BioDbEntry$methods(	getId = function() {
	return(.self$.id)
})

###########
# KEGG ID #
###########

BioDbEntry$methods(	getKeggId = function() {
	return(.self$.keggid)
})

#########
# INCHI #
#########

BioDbEntry$methods(	getInchi = function() {
	return(.self$.inchi)
})

#############
# INCHI KEY #
#############

BioDbEntry$methods(	getInchiKey = function() {
	return(.self$.inchikey)
})
