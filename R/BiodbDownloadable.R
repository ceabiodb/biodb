# vi: fdm=marker

# Class declaration {{{1
################################################################

BiodbDownloadable <- methods::setRefClass("BiodbDownloadable", contains = 'BiodbObject', fields = list(.ext = 'character'))

# Constructor {{{1
################################################################0

BiodbDownloadable$methods( initialize = function(...) {

	callSuper(...)

	.ext <<- NA_character_
})

# Set download extension {{{1
################################################################

BiodbDownloadable$methods( .setDownloadExt = function(ext) {
	.ext <<- ext
})

# Get download path {{{1
################################################################

BiodbDownloadable$methods( getDownloadPath = function() {
	# TODO Massbank.eu needs to download again the db, since target name is different.
	return(.self$getBiodb()$getCache()$getFilePath(dbid = .self$getId(), subfolder = 'longterm', name = 'download', ext = .self$.ext))
})

# Is downloaded {{{1
################################################################

BiodbDownloadable$methods( isDownloaded = function() {
	return(file.exists(.self$getDownloadPath()))
})

# Is extracted {{{1
################################################################

BiodbDownloadable$methods( isExtracted = function() {
	return(.self$getBiodb()$getCache()$markerExist(dbid = .self$getId(), subfolder = 'shortterm', name = 'extracted'))
})

# Download {{{1
################################################################

BiodbDownloadable$methods( download = function() {

	# Download
	if ( ! .self$isDownloaded() && .self$getBiodb()$getConfig()$isEnabled('allow.huge.downloads') && ! .self$getBiodb()$getConfig()$get('offline')) {

		.self$message(MSG.INFO, paste("Download whole database of ", .self$getId(), ".", sep = ''))

		.self$.doDownload()
	}

	# Extract
	if (.self$isDownloaded() && ! .self$isExtracted()) {

		.self$message(MSG.INFO, paste("Extract whole database of ", .self$getId(), ".", sep = ''))

		.self$.doExtractDownload()

		# Set marker
		.self$getBiodb()$getCache()$setMarker(dbid = .self$getId(), subfolder = 'shortterm', name = 'extracted')
	}
})

# Do download {{{1
################################################################

BiodbDownloadable$methods( .doDownload = function() {
	.self$.abstract.method()
})
