########################
# Entry ABSTRACT CLASS #
########################

BioDbEntry <- setRefClass("BioDbEntry", fields = list(.id = "character", .keggid = "character", .inchi = "character", .inchikey = "character"))

###############
# CONSTRUCTOR #
###############

BioDbEntry$methods( initialize = function(id = NA_character_, ...) {

	.id <<- if ( ! is.null(id)) id else NA_character_
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

BioDbEntry$methods( getKeggId = function() {
	stop("Method getKeggId() is not implemented in concrete class.")
})

################
# LIPIDMAPS ID #
################

BioDbEntry$methods( getLipidmapsId = function() {
	stop("Method getLipidmapsId() is not implemented in concrete class.")
})

#########
# INCHI #
#########

BioDbEntry$methods(	getInchi = function() {
	stop("Method getInchi() is not implemented in concrete class.")
})

#############
# INCHI KEY #
#############

BioDbEntry$methods(	getInchiKey = function() {
	stop("Method getInchi() is not implemented in concrete class.")
})
