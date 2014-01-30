source('__UrlRequestScheduler.R')

#####################
# CLASS DECLARATION #
#####################

BioDbConn <- setRefClass("BioDbConn",
						   fields = list(useragent = "character", scheduler="UrlRequestScheduler"))

###############
# CONSTRUCTOR #
###############

BioDbConn$methods( initialize = function(useragent = "", scheduler = UrlRequestScheduler$new(n = 3), ...) {
	useragent <<- useragent
	scheduler <<- scheduler
	callSuper(...) # calls super-class initializer with remaining parameters
})

###########
# GET URL #
###########

BioDbConn$methods( getUrl = function(url) {
	return(.self$scheduler$getUrl(url, useragent = .self$useragent))
})
