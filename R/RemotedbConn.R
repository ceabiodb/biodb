if ( ! exists('RemotedbConn')) {

	#####################
	# CLASS DECLARATION #
	#####################
	
	RemotedbConn <- methods::setRefClass("RemotedbConn", contains = "BiodbConn", fields = list(.scheduler = "UrlRequestScheduler", .token = "character"))

	###############
	# CONSTRUCTOR #
	###############

	RemotedbConn$methods( initialize = function(scheduler = NULL, token = NA_character_, ...) {

		# Set token
		.token <<- token

		# Set scheduler
		if (is.null(scheduler))
			scheduler <- UrlRequestScheduler$new(n = 3, parent = .self)
		is(scheduler, "UrlRequestScheduler") || .self$message(MSG.ERROR, "The scheduler instance must inherit from UrlRequestScheduler class.")
		.scheduler <<- scheduler
	
		callSuper(...) # calls super-class initializer with remaining parameters
	})

	###########
	# GET URL #
	###########

	RemotedbConn$methods( .get.url = function(url) {
		return(.self$.scheduler$getUrl(url))
	})
	
	###########
	# GET URL #
	###########
	
	RemotedbConn$methods( .set.useragent = function(useragent) {
		.scheduler$setUserAgent(useragent) # set agent
	})

}
