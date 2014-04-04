########################
# Entry ABSTRACT CLASS #
########################

Entry <- setRefClass("Entry")

######
# ID #
######

Entry$methods(
	getId = function() {
		return(NA_character_)
	}
)

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
		return(NA_character_)
	}
)
