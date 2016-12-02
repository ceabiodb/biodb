##########################
# CLASS DECLARATION {{{1 #
##########################

BiodbObject <- methods::setRefClass("BiodbObject", fields = list( .debug = "logical" ))

########################
# ABSTRACT METHOD {{{1 #
########################

BiodbObject$methods( .abstract.method = function() {

	class <- class(.self)
	method <- sys.call(length(sys.calls()) - 1)
	method <- sub('^[^$]*\\$([^(]*)\\(.*$', '\\1()', method)

	stop(paste("Method", method, "is not implemented in", class, "class."))
})
