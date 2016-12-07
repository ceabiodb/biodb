#####################
# CLASS DECLARATION #
#####################

BiodbConn <- methods::setRefClass("BiodbConn", contains = "BiodbObject", fields = list( .debug = "logical" ))

###############
# CONSTRUCTOR #
###############

BiodbConn$methods( initialize = function(debug = FALSE, ...) {
	.debug <<- debug
	callSuper(...)
})

##########################
# GET ENTRY CONTENT TYPE #
##########################

BiodbConn$methods( getEntryContentType = function() {
	.self$.abstract.method()
})

#############
# GET ENTRY #
#############

BiodbConn$methods( getEntry = function(id, drop = TRUE) {
	content <- .self$getEntryContent(id)
	return(.self$createEntry(content, drop = drop))
})

#####################
# GET ENTRY CONTENT #
#####################

# Download entry content from the public database.
# type      The entry type.
# id        The ID of the entry to get.
# RETURN    An entry content downloaded from database.
BiodbConn$methods( getEntryContent = function(id) {
	.self$.abstract.method()
})

#############################
# CREATE ENTRY FROM CONTENT #
#############################

# Creates a Compound instance from file content.
# content       A file content, downloaded from the public database.
# RETURN        A compound instance.
BiodbConn$methods( createEntry = function(content, drop = TRUE) {
	.self$.abstract.method()
})

#################
# GET ENTRY IDS #
#################

# Get a list of IDs of all entries contained in this database.
BiodbConn$methods( getEntryIds = function(max.results = NA_integer_) {
	.self$.abstract.method()
})

##################
# GET NB ENTRIES #
##################

# Get the number of entries contained in this database.
BiodbConn$methods( getNbEntries = function() {
	.self$.abstract.method()
})
