########################
# Entry ABSTRACT CLASS #
########################

Entry <- setRefClass("Entry")

########
# SAVE #
########

Entry$methods(
	save = function(file) {
		# do nothing
	}
)

###########
# KEGG ID #
###########

Entry$methods(
	getKeggId = function() {
		return(NULL)
	}
)
