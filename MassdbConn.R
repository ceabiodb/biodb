if ( ! exists('MassdbConn')) {

	source('BiodbConn.R')

	#####################
	# CLASS DECLARATION #
	#####################
	
	MassdbConn <- setRefClass("MassdbConn", contains = "BiodbConn")

	###############################
	# GET CHROMATOGRAPHIC COLUMNS #
	###############################
	
	# Get a list of chromatographic columns contained in this database.
	# compound.ids  A list of compound IDs used to filter results.
	# The returned value is a data.frame with two columns : one for the ID (BIODB.ID) and another one for the title (BIODB.TITLE).
	MassdbConn$methods( getChromCol = function(compound.ids = NULL) {
		stop("Method getChromCol() is not implemented in concrete class.")
	})

	#################
	# GET MZ VALUES #
	#################
	
	# Returns a numeric vector of all masses stored inside the database.
	MassdbConn$methods( getMzValues = function(mode = NULL, max.results = NA_integer_) {
		stop("Method getMzValues() not implemented in concrete class.")
	})

	################
	# GET NB PEAKS #
	################
	
	# Returns the number of peaks contained in the database
	MassdbConn$methods( getNbPeaks = function(mode = NULL, compound.ids = NULL) {
		stop("Method getMzValues() not implemented in concrete class.")
	})

	#########################
	# FIND COMPOUND BY NAME #
	#########################

	# Find a molecule by name
	# name   A vector of molecule names to search for.
	# Return an integer vector of the same size as the name input vector, containing the found molecule IDs, in the same order.
	MassdbConn$methods( findCompoundByName = function(name) {
		stop("Method getMzValues() not implemented in concrete class.")
	})
}
