library("stringr")
source('Entry.R')

###########################
# TxtEntry ABSTRACT CLASS #
###########################

TxtEntry <- setRefClass("TxtEntry", contains = "Entry", fields = list(id = "character", .orig_text_file = "character"))

###############
# CONSTRUCTOR #
###############

TxtEntry$methods(
	initialize = function(id = NA_character_, .orig_text_file = NA_character_, ...) {
		id <<- id
		.orig_text_file <<- .orig_text_file
		callSuper(...) # calls super-class initializer with remaining parameters
	}
)

######
# ID #
######

TxtEntry$methods(
	getId = function() {
		return(.self$id)
	}
)

########
# SAVE #
########

TxtEntry$methods(
	save = function(filename) {
		fileConn<-file(filename)
		writeLines(.self$.orig_text_file, fileConn)
		close(fileConn)
	}
)
