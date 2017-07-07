# vi: fdm=marker

# Class declaration {{{1
################################################################

#' A class for describing the characteristics of a database.
#'
#' This class is used by \code{\link{BiodbDbsInfo}} for storing database characteristics.
#'
#' @seealso \code{\link{BiodbDbsInfo}}.
#'
#' @import methods
#' @include ChildObject.R
#' @export BiodbDbInfo
#' @exportClass BiodbDbInfo
BiodbDbInfo <- methods::setRefClass("BiodbDbInfo", contains =  "ChildObject", fields = list( .name = "character"))

# Constructor {{{1
################################################################

BiodbDbInfo$methods( initialize = function(name, ...) {

	callSuper(...)

	# Set name
	if ( is.null(name) || is.na(name) || nchar(name) == '')
		.self$message(MSG.ERROR, "You cannot set an empty name for a database. Name was empty (either NULL or NA or empty string).")
	.name <<- name

})

# Get connection class name {{{1
################################################################

BiodbDbInfo$methods( getConnClassName = function() {

    # Get connection class name
    s <- .self$.name
	indices <- as.integer(gregexpr('\\.[a-z]', .self$.name, perl = TRUE)[[1]])
    indices <- indices + 1  # We are interested in the letter after the dot.
    indices <- c(1, indices) # Add first letter.
	for (i in indices)
		s <- paste(substring(s, 1, i - 1), toupper(substring(s, i, i)), substring(s, i + 1), sep = '')
    s <- gsub('.', '', s, fixed = TRUE) # Remove dots
	conn.class.name <- paste(s, 'Conn', sep = '')

	return(conn.class.name)
})

# Get connection class {{{1
################################################################

BiodbDbInfo$methods( getConnClass = function() {
	return(get(.self$getConnClassName()))
})

# Get entry class name {{{1
################################################################

BiodbDbInfo$methods( getEntryClassName = function() {

    # Get entry class name
	s <- .self$.name
	indices <- as.integer(gregexpr('\\.[a-z]', .self$.name, perl = TRUE)[[1]])
	indices <- indices + 1  # We are interested in the letter after the dot.
	indices <- c(1, indices) # Add first letter.
	for (i in indices)
		s <- paste(substring(s, 1, i - 1), toupper(substring(s, i, i)), substring(s, i + 1), sep = '')
	s <- gsub('.', '', s, fixed = TRUE) # Remove dots
	entry.class.name <- paste(s, 'Entry', sep = '')

	return(entry.class.name)
})

# Get entry class {{{1
################################################################

BiodbDbInfo$methods( getEntryClass = function() {
	return(get(.self$getEntryClassName()))
})
