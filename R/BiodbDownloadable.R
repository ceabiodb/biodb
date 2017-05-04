# vi: fdm=marker

# Class declaration {{{1
################################################################

BiodbDownloadable <- methods::setRefClass("BiodbDownloadable", contains = 'BiodbObject')

# Constructor {{{1
################################################################0

BiodbDownloadable$methods( initialize = function(...) {

	callSuper(...)
})

# Is downloaded {{{1
################################################################

BiodbDownloadable$methods( isDownloaded = function() {
	return(.self$getBiodb()$getCache()$markerExists(db = .self$getId(), folder = CACHE.SHORT.TERM.FOLDER, name = 'extracted'))
})

# Download {{{1
################################################################

BiodbDownloadable$methods( download = function() {

	if ( ! .self$isDownloaded() && .self$getBiodb()$getConfig()$isEnabled(CFG.ALLOW.HUGE.DOWNLOADS))
		if (.self$.doDownload())
			# Set marker
			.self$getBiodb()$getCache()$setMarker(db = .self$getId(), folder = CACHE.SHORT.TERM.FOLDER, name = 'extracted')
})

# Do download {{{1
################################################################

BiodbDownloadable$methods( .doDownload = function() {
	.self$.abstract.method()
})
