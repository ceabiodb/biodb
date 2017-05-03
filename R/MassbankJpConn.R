# vi: fdm=marker

#' @include MassbankConn.R
#' @include BiodbDownloadable.R

# Constants {{{1
################################################################

MASSBANK.JP.URL  <- 'http://www.massbank.jp/'

# Class declaration {{{1
################################################################

MassbankJpConn <- methods::setRefClass("MassbankJpConn", contains = c('MassbankConn', 'BiodbDownloadable'))

# Constructor {{{1
################################################################0

MassbankJpConn$methods( initialize = function(...) {

	callSuper(base.url = MASSBANK.JP.URL, ...)
})

# Do download {{{1
################################################################

MassbankJpConn$methods( .doDownload = function() {

	.self$message(MSG.INFO, "Extract whole MassBank database.")

	# SVN export
	svn.path <- .self$getBiodb()$getCache()$getFilePaths(db = .self$getId(), folder = CACHE.LONG.TERM.FOLDER, names = 'download', ext = 'svn')
	if ( ! .self$getBiodb()$getConfig()$get(CFG.OFFLINE) && ! file.exists(svn.path)) {
		.self$message(MSG.INFO, "Download whole MassBank database from SVN server.")
		svn.cmd <- .self$getBiodb()$getConfig()$get(CFG.SVN.BINARY.PATH)
		system2(svn.cmd, c('export', '--force', '--quiet', 'http://www.massbank.jp/SVN/OpenData/record/', svn.path))
	}

	# Copy all exported files
	.self$message(MSG.INFO, "Copy all MassBank record files from SVN local export directory into cache.")
	svn.files <- Sys.glob(file.path(svn.path, '*', '*.txt'))
	.self$message(MSG.INFO, paste("Found ", length(svn.files), " record files in MassBank SVN local export directory."))
	ids <- sub('^.*/([^/]*)\\.txt$', '\\1', svn.files)
	dup.ids <- duplicated(ids)
	if (any(dup.ids))
		.self$message(MSG.CAUTION, paste("Found duplicated IDs in downloaded Massbank records: ", paste(ids[dup.ids], collapse = ', '), '.', sep = ''))
	cache.files <- .self$getBiodb()$getCache()$getFilePaths(db = .self$getId(), folder = CACHE.SHORT.TERM.FOLDER, names = ids, ext = .self$getEntryContentType())
	.self$getBiodb()$getCache()$deleteFiles(db = .self$getId(), folder = CACHE.SHORT.TERM.FOLDER, ext = .self$getEntryContentType())
	file.copy(svn.files, cache.files)

	return(TRUE)
})

# Get entry ids {{{1
################################################################

MassbankJpConn$methods( getEntryIds = function(max.results = NA_integer_) {

	ids <- NULL

	# Do we allow database download? This can take some time.
	if (.self$getBiodb()$getConfig()$isEnabled(CFG.ALLOW.HUGE.DOWNLOADS)) {

		# Download
		.self$download()

		# Get IDs from cache
		ids <- .self$getBiodb()$getCache()$listFiles(db = .self$getId(), folder = CACHE.SHORT.TERM.FOLDER, ext = .self$getEntryContentType(), extract.names = TRUE)

		# Cut
		if ( ! is.na(max.results) && max.results < length(ids))
			ids <- ids[1:max.results]
	}

	return(ids)
})

# Get nb entries {{{1
################################################################

MassbankJpConn$methods( getNbEntries = function(count = FALSE) {
	return(length(.self$getEntryIds()))
})
