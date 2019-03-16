# vi: fdm=marker

# Class declaration {{{1
################################################################

#' An abstract class (more like an interface) to model a remote database that can be downloaded locally.
#'
#' The class must be inherited by any remote database class that allows download of its whole content. The hidden/private method \code{.doDownload()} must be implemented by the database class.
#'
#' @seealso \code{\link{BiodbRemotedbConn}}.
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
	.self$.abstract.class('BiodbDownloadable')

	.ext <<- NA_character_
})

# Get download path {{{1
################################################################

BiodbDownloadable$methods( getDownloadPath = function() {
	":\n\nGet the path where the downloaded containt is written."

	path <- .self$getBiodb()$getCache()$getFilePath(.self$getCacheId(), subfolder = 'longterm', name = 'download', ext = .self$.ext)

	.self$message('debug', paste0('Download path of ', .self$getId(), ' is "', path, '".'))

	return(path)
})

# Requires download {{{1
################################################################

BiodbDownloadable$methods( requiresDownload = function() {
	return(FALSE)
})

# Is downloaded {{{1
################################################################

BiodbDownloadable$methods( isDownloaded = function() {
	":\n\nReturns TRUE if the database containt has already been downloaded."

	already.downloaded <- file.exists(.self$getDownloadPath())

	.self$message('debug', paste0('Database ', .self$getId(), ' has ', (if (already.downloaded) 'already' else 'not yet'), ' been downloaded.'))

	return(already.downloaded)
})

# Is extracted {{{1
################################################################

BiodbDownloadable$methods( isExtracted = function() {
	":\n\nReturns TRUE of the downloaded database containt has been extracted."

	return(.self$getBiodb()$getCache()$markerExist(.self$getCacheId(), subfolder = 'shortterm', name = 'extracted'))
})

# Download {{{1
################################################################

BiodbDownloadable$methods( download = function() {
	":\n\nDownload the database containt locally."

	# Download
	if ( ! .self$isDownloaded() && (.self$getBiodb()$getConfig()$isEnabled('allow.huge.downloads') || .self$requiresDownload()) && ! .self$getBiodb()$getConfig()$isEnabled('offline')) {

		.self$message('info', paste("Download whole database of ", .self$getId(), ".", sep = ''))

		.self$.doDownload()
	}

	# Extract
	if (.self$isDownloaded() && ! .self$isExtracted()) {

		.self$message('info', paste("Extract whole database of ", .self$getId(), ".", sep = ''))

		.self$.doExtractDownload()

		# Set marker
		.self$getBiodb()$getCache()$setMarker(.self$getCacheId(), subfolder = 'shortterm', name = 'extracted')
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
