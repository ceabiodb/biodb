# vi: fdm=marker

#' include BiodbEntry.R

# Class declaration {{{1
################################################################

TextEntry <- methods::setRefClass("TextEntry", contains = "BiodbEntry")

## Constructor {{{1
#################################################################
#
#TextEntry$methods( initialize = function(...) {
#
#	callSuper(...)
#})
#
## Parse content {{{1
#################################################################
#
#TextEntry$methods( parseContent = function(content) {
#
#	if (.self$beforeParseContent(xml)) {
#	}
#
#	.self$afterParseContent(xml)
#})
#
## Before parse content {{{1
#################################################################
#
#TextEntry$methods( beforeParseContent = function(xml) {
#	return(TRUE)
#})
#
## After parse content {{{1
#################################################################
#
#TextEntry$methods( afterParseContent = function(xml) {
#})
