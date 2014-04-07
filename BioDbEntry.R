########################
# Entry ABSTRACT CLASS #
########################

BioDbEntry <- setRefClass("BioDbEntry", fields = list(id = "character", kegg_id = "character"))

###############
# CONSTRUCTOR #
###############

BioDbEntry$methods( initialize = function(id = NA_character_, kegg_id = NA_character_, ...) {
	id <<- id
	kegg_id <<- kegg_id
	callSuper(...) # calls super-class initializer with remaining parameters
})

######
# ID #
######

BioDbEntry$methods(
	getId = function() {
		return(.self$id)
	}
)

###########
# KEGG ID #
###########

BioDbEntry$methods(
	getKeggId = function() {
		return(.self$kegg_id)
	}
)
