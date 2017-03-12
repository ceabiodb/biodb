# vi: fdm=marker

#' @include biodb-common.R

# Constants {{{1
################################################################

# Cardinalities
BIODB.CARD.ONE <- '1'
BIODB.CARD.MANY <- '*'
FIELD.CARDINALITIES <- c(BIODB.CARD.ONE, BIODB.CARD.MANY)

FIELD.CLASSES <- c('character', 'integer', 'double', 'logical', 'object', 'data.frame')

# Class declaration {{{1
################################################################

BiodbEntryField <- methods::setRefClass("BiodbEntryField", contains = "BiodbObject", fields = list(  .parent = "ANY", .name = 'character', .class = 'character', .cardinality = 'character', .allow.duplicates = 'logical'))

# constructor {{{1
################################################################

BiodbEntryField$methods( initialize = function(parent, name, class = 'character', card = BIODB.CARD.ONE, allow.duplicates = FALSE, ...) {

	callSuper(...)

	.parent <<- parent

	# Set name
	if ( is.null(name) || is.na(name) || nchar(name) == '')
		.self$message(MSG.ERROR, "You cannot set an empty name for a field. Name was empty (either NULL or NA or empty string).")
	.name <<- name

	# Set class
	if ( ! class %in% FIELD.CLASSES)
		.self$message(MSG.ERROR, paste("Unknown class \"", class, "\" for field \"", name, "\".", sep = ''))
	.class <<- class

	# Set cardinality
	if ( ! card %in% FIELD.CARDINALITIES)
		.self$message(MSG.ERROR, paste("Unknown cardinality \"", card, "\" for field \"", name, "\".", sep = ''))
	.cardinality <<- card

	# Set other fields
	.allow.duplicates <<- allow.duplicates
})

# Get biodb {{{1
################################################################

BiodbEntryField$methods( getBiodb = function() {
	return(.self$.parent$getBiodb())
})

# Has card one {{{1
################################################################

BiodbEntryField$methods( hasCardOne = function() {
	return(.self$.cardinality == BIODB.CARD.MANY)
})

# Has card many {{{1
################################################################

BiodbEntryField$methods( hasCardMany = function() {
	return(.self$.cardinality == BIODB.CARD.MANY)
})

# Card is many {{{1
################################################################

BiodbEntryField$methods( cardIsMany = function() {
	return(.self$.cardinality == BIODB.CARD.MANY)
})

# Allows duplicates {{{1
################################################################

BiodbEntryField$methods( allowsDuplicates = function() {
	return(.self$.allow.duplicates)
})

# Get class {{{1
################################################################

BiodbEntryField$methods( getClass = function() {
	return(.self$.class)
})

# Is object {{{1
################################################################

BiodbEntryField$methods( isObject = function() {
	return(.self$.class == 'object')
})

# Is data frame {{{1
################################################################

BiodbEntryField$methods( isDataFrame = function() {
	return(.self$.class == 'data.frame')
})

# Is vector {{{1
################################################################

BiodbEntryField$methods( isVector = function() {
	return(.self$.class %in% c('character', 'integer', 'double', 'logical'))
})

# DEPRECATED METHODS {{{1
################################################################

# Get cardinality {{{2
################################################################

BiodbEntryField$methods( getCardinality = function() {
	.self$.deprecated.method('hasCardOne() or hasCardMany()')
	return(.self$.cardinality)
})
