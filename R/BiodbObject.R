##########################
# CLASS DECLARATION {{{1 #
##########################

BiodbObject <- methods::setRefClass("BiodbObject", fields = list( .debug = "logical" ))

########################
# ABSTRACT METHOD {{{1 #
########################

BiodbObject$methods( .abstract.method = function() {

	class <- class(.self)
	method <- sub('^[^$]*\\$([^(]*)\\(.*$', '\\1()', traceback(0)[[5]])

	stop(paste("Method", method, "is not implemented in", class, "class."))
})
