########################
# Entry ABSTRACT CLASS #
########################

BiodbEntry <- setRefClass("BiodbEntry", fields = list(.id = "character", .factory = "ANY"))

###############
# CONSTRUCTOR #
###############

BiodbEntry$methods( initialize = function(id = NA_character_, ...) {

	.id <<- if ( ! is.null(id)) id else NA_character_
	.factory <<- NULL

	callSuper(...) # calls super-class initializer with remaining parameters
})

######
# ID #
######

BiodbEntry$methods(	getId = function() {
	return(.self$.id)
})

###########
# FACTORY #
###########

BiodbEntry$methods(	setFactory = function(factory) {
	.factory <<- factory
})

########
# NAME #
########

BiodbEntry$methods( getName = function() {
	stop("Method getName() is not implemented in concrete class.")
})

############
# CHEBI ID #
############

BiodbEntry$methods( getChebiId = function() {
	stop("Method getChebiId() is not implemented in concrete class.")
})

###########
# KEGG ID #
###########

BiodbEntry$methods( getKeggId = function() {
	stop("Method getKeggId() is not implemented in concrete class.")
})

################
# LIPIDMAPS ID #
################

BiodbEntry$methods( getLipidmapsId = function() {
	stop("Method getLipidmapsId() is not implemented in concrete class.")
})

#########
# INCHI #
#########

BiodbEntry$methods( getInchi = function() {
	stop("Method getInchi() is not implemented in concrete class.")
})

#############
# INCHI KEY #
#############

BiodbEntry$methods(	getInchiKey = function() {
	stop("Method getInchi() is not implemented in concrete class.")
})
