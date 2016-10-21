if ( ! exists('RemotedbConn')) {

	source('BiodbConn.R')
	source('UrlRequestScheduler.R')

	#####################
	# CLASS DECLARATION #
	#####################
	
	RemotedbConn <- setRefClass("RemotedbConn", contains = "BiodbConn", fields = list(.scheduler = "UrlRequestScheduler", .token = "character"))

	###############
	# CONSTRUCTOR #
	###############

	RemotedbConn$methods( initialize = function(useragent = NA_character_, scheduler = NULL, token = NA_character_, ...) {

		# Check useragent
		( ! is.null(useragent) && ! is.na(useragent)) || stop("You must specify a valid useragent string (e.g.: \"myapp ; my.email@address\").")

		# Set token
		.token <<- token

		# Set scheduler
		if (is.null(scheduler))
			scheduler <- UrlRequestScheduler$new(n = 3)
		inherits(scheduler, "UrlRequestScheduler") || stop("The scheduler instance must inherit from UrlRequestScheduler class.")
		scheduler$setUserAgent(useragent) # set agent
		.scheduler <<- scheduler
	
		callSuper(...) # calls super-class initializer with remaining parameters
	})

	###########
	# GET URL #
	###########

	RemotedbConn$methods( .get.url = function(url) {
		.self$.print.debug.msg(paste0("Sending URL request '", url, "'..."))
		return(.self$.scheduler$getUrl(url))
	})

}
