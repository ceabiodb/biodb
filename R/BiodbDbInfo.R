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
