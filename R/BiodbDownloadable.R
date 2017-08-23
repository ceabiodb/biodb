# vi: fdm=marker

# Class declaration {{{1
################################################################

#' An abstract class (more like an interface) to modelise a remote database that can be downloaded locally.
#'
#' The class must be inherited by any remote database class that allows download of its whole content. The hidden/private method \code{.doDownload()} must be implemented by the database class.
#'
#' @seealso \code{\link{RemotedbConn}}.
#'
#' @import methods
#' @include BiodbObject.R
#' @export BiodbDownloadable
#' @exportClass BiodbDownloadable
BiodbDownloadable <- methods::setRefClass("BiodbDownloadable", contains = 'BiodbObject', fields = list(.ext = 'character'))

# Constructor {{{1
################################################################0

BiodbDownloadable$methods( initialize = function(...) {

	callSuper(...)

	.ext <<- NA_character_
})

# Get download path {{{1
################################################################

BiodbDownloadable$methods( getDownloadPath = function() {
	":\n\nGet the path where the downloaded containt is written."

	# TODO Massbank.eu needs to download again the db, since target name is different.
	return(.self$getBiodb()$getCache()$getFilePath(dbid = .self$getId(), subfolder = 'longterm', name = 'download', ext = .self$.ext))
})

# Is downloaded {{{1
################################################################

BiodbDownloadable$methods( isDownloaded = function() {
	":\n\nReturns TRUE if the database containt has already been downloaded."

	return(file.exists(.self$getDownloadPath()))
})

# Is extracted {{{1
################################################################

BiodbDownloadable$methods( isExtracted = function() {
	":\n\nReturns TRUE of the downloaded database containt has been extracted."

	return(.self$getBiodb()$getCache()$markerExist(dbid = .self$getId(), subfolder = 'shortterm', name = 'extracted'))
})

# Download {{{1
################################################################

BiodbDownloadable$methods( download = function() {
	":\n\nDownload the database containt locally."

	# Download
	if ( ! .self$isDownloaded() && .self$getBiodb()$getConfig()$isEnabled('allow.huge.downloads') && ! .self$getBiodb()$getConfig()$get('offline')) {

		.self$message('info', paste("Download whole database of ", .self$getId(), ".", sep = ''))

		.self$.doDownload()
	}

	# Extract
	if (.self$isDownloaded() && ! .self$isExtracted()) {

		.self$message('info', paste("Extract whole database of ", .self$getId(), ".", sep = ''))

		.self$.doExtractDownload()

		# Set marker
		.self$getBiodb()$getCache()$setMarker(dbid = .self$getId(), subfolder = 'shortterm', name = 'extracted')
	}
})

# Private methods {{{1
################################################################

# Set download extension {{{2
################################################################

BiodbDownloadable$methods( .setDownloadExt = function(ext) {
	.ext <<- ext
})

# Do download {{{2
################################################################

BiodbDownloadable$methods( .doDownload = function() {
	.self$.abstract.method()
})
