if ( ! exists('BiodbCompound')) { # Do not load again if already loaded

	########################
	# Compound ABSTRACT CLASS #
	########################
	
	BiodbCompound <- setRefClass("BiodbCompound", fields = list(.id = "character", .factory = "ANY"))
	
	###############
	# CONSTRUCTOR #
	###############
	
	BiodbCompound$methods( initialize = function(id = NA_character_, ...) {
	
		.id <<- if ( ! is.null(id)) id else NA_character_
		.factory <<- NULL
	
		callSuper(...) # calls super-class initializer with remaining parameters
	})
	
	######
	# ID #
	######
	
	BiodbCompound$methods(	getId = function() {
		return(.self$.id)
	})
	
	###########
	# FACTORY #
	###########
	
	BiodbCompound$methods(	setFactory = function(factory) {
		.factory <<- factory
	})
	
	########
	# NAME #
	########
	
	BiodbCompound$methods( getName = function() {
		stop("Method getName() is not implemented in concrete class.")
	})
	
	############
	# CHEBI ID #
	############
	
	BiodbCompound$methods( getChebiId = function() {
		stop("Method getChebiId() is not implemented in concrete class.")
	})
	
	###########
	# KEGG ID #
	###########
	
	BiodbCompound$methods( getKeggId = function() {
		stop("Method getKeggId() is not implemented in concrete class.")
	})
	
	##############
	# PUBCHEM ID #
	##############
	
	BiodbCompound$methods( getPubchemId = function() {
		stop("Method getPubchemId() is not implemented in concrete class.")
	})
	
	################
	# LIPIDMAPS ID #
	################
	
	BiodbCompound$methods( getLipidmapsId = function() {
		stop("Method getLipidmapsId() is not implemented in concrete class.")
	})
	
	#########
	# INCHI #
	#########
	
	BiodbCompound$methods( getInchi = function() {
		stop("Method getInchi() is not implemented in concrete class.")
	})
	
	#############
	# INCHI KEY #
	#############
	
	BiodbCompound$methods(	getInchiKey = function() {
		stop("Method getInchi() is not implemented in concrete class.")
	})

	###############
	# GET SPECTRA #
	###############

	BiodbCompound$methods( getSpectra = function() {
		stop("Method getSpectra() is not impemented in concrete class.")
	})
}
