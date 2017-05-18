# vi: fdm=marker

# Constants {{{1
################################################################

CACHE.SHORT.TERM.FOLDER <- 'shortterm'
CACHE.LONG.TERM.FOLDER <- 'longterm'
CACHE.FOLDERS <- c(CACHE.SHORT.TERM.FOLDER, CACHE.LONG.TERM.FOLDER)

# Class declaration {{{1
################################################################

#' @import methods
#' @include ChildObject.R
BiodbCache <- methods::setRefClass("BiodbCache", contains = 'ChildObject')

# Constructor {{{1
################################################################

BiodbCache$methods( initialize = function(...) {

	callSuper(...)
})

# Get directory {{{1
################################################################

BiodbCache$methods( getDir = function() {

	cachedir <- .self$getBiodb()$getConfig()$get(CFG.CACHE.DIRECTORY)

	# Create cache dir if needed
	if ( ! is.na(cachedir) && ! file.exists(cachedir))
		dir.create(cachedir)

	return(cachedir)
})

# Is cache reading enabled {{{1
################################################################

BiodbCache$methods( isReadable = function() {
	return( .self$getBiodb()$getConfig()$isEnabled('cache.system') && ! is.na(.self$getDir()))
})

# Is cache writing enabled {{{1
################################################################

BiodbCache$methods( isWritable = function() {
	return( .self$getBiodb()$getConfig()$isEnabled('cache.system') && ! is.na(.self$getDir()) && ! .self$getBiodb()$getConfig()$get(CFG.CACHE.READ.ONLY))
})

# File exists {{{1
################################################################

BiodbCache$methods( fileExists = function(db, folder, names, ext) {

	exists <- file.exists(.self$getFilePaths(db, folder, names, ext))

	return(exists)
})

# Marker exists {{{1
################################################################

BiodbCache$methods( markerExists = function(db, folder, name) {
	return(.self$fileExists(db = db, folder = folder, names = name, ext = 'marker'))
})

# Set marker {{{1
################################################################

BiodbCache$methods( setMarker = function(db, folder, name) {

	marker.path <- .self$getFilePaths(db = db, folder = folder, names = name, ext = 'marker')

	writeChar('', marker.path)
})

# Get file paths {{{1
################################################################

BiodbCache$methods( getFilePaths = function(db, folder, names, ext) {

	# Set file paths
	filepaths <- file.path(.self$getFolderPath(folder), paste(db, '-', names, '.', ext, sep = ''))

	# Set NA values
	filepaths[is.na(names)] <- NA_character_

	return(filepaths)
})

# Load file content {{{1
################################################################

BiodbCache$methods( loadFileContent = function(db, folder, names, ext, output.vector = FALSE) {

	if ( ! .self$isReadable())
		.self$message(MSG.ERROR, paste("Attempt to read from non-readable cache \"", .self$getDir(), "\".", sep = ''))

	content <- NULL

	# Read contents from files
	file.paths <- .self$getBiodb()$getCache()$getFilePaths(db, folder, names, ext)
	.self$message(MSG.DEBUG, paste("Loading from cache \"", paste(if (length(file.paths) > 10) c(file.paths[1:10], '...') else file.paths, collapse = ", ") ,"\".", sep = ''))
	content <- lapply(file.paths, function(x) { if (is.na(x)) NA_character_ else ( if (file.exists(x)) readChar(x, file.info(x)$size, useBytes = TRUE) else NULL )} )

	# Check that the read content is not conflicting with the current locale
	for (i in seq(content)) {
		n <- tryCatch(nchar(content[[i]]), error = function(e) NULL)
		if (is.null(n))
			.self$message(MSG.ERROR, paste("Impossible to handle correctly the content of file \"", file.paths[[i]], "\". Your current locale might be conflicting with functions like `nchar` or `strsplit`. Please, set your locale to 'C' (`Sys.setlocale(locale = 'C')`), or enable option `` in biodb configuration.", sep = ''))
	}

	# Set to NA
	content[content == 'NA' | content == "NA\n"] <- NA_character_

	# Vector ?
	if (output.vector)
		content <- vapply(content, function(x) if (is.null(x)) NA_character_ else x, FUN.VALUE = '')

	return(content)
})

# Save content into file {{{1
################################################################

BiodbCache$methods( saveContentToFile = function(contents, db, folder, names, ext) {

	if ( ! .self$isWritable())
		.self$message(MSG.ERROR, paste("Attempt to write into non-writable cache. \"", .self$getDir(), "\".", sep = ''))

	# Get file paths
	file.paths <- .self$getBiodb()$getCache()$getFilePaths(db, folder, names, ext)

	# Check that we have the same number of contents and file paths
	if (length(file.paths) != length(contents))
		.self$message(MSG.ERROR, paste("The number of contents to save (", length(contents), ") is different from the number of paths (", length(file.paths), ").", sep = ''))

	# Replace NA values with 'NA' string
	contents[is.na(contents)] <- 'NA'

	# Write contents to files
	.self$message(MSG.DEBUG, paste("Saving to cache \"", paste(if (length(file.paths) > 10) c(file.paths[1:10], '...') else file.paths, collapse = ", ") ,"\".", sep = ''))
	mapply(function(c, f) { if ( ! is.null(c)) cat(c, file = f) }, contents, file.paths) # Use cat instead of writeChar, because writeChar was not working with some unicode string (wrong string length).
})

# Get folder path {{{1
################################################################

BiodbCache$methods( getFolderPath = function(folder) {

	# Check folder
	if ( ! folder %in% CACHE.FOLDERS)
		.self$message(MSG.ERROR, paste("Unknown cache folder \"", folder, "\".", sep = ''))

	# Get folder path
	if (.self$getBiodb()$getConfig()$isEnabled(CFG.USE.CACHE.SUBFOLDERS))
		folder.path <- file.path(.self$getDir(), folder)
	else
		folder.path <- .self$getDir()

	# Create folder if needed
	if ( ! is.na(folder.path) && ! file.exists(folder.path))
		dir.create(folder.path)

	return(folder.path)
})

# Delete files {{{1
################################################################

BiodbCache$methods( deleteFiles = function(db, folder, ext = NA_character_) {

	files <- paste(db, '*',sep = '-')
	if ( ! is.na(ext))
		files <- paste(files, ext, sep = '.')

	unlink(file.path(.self$getFolderPath(folder), files))
})

# List files {{{1
################################################################

BiodbCache$methods( listFiles = function(db, folder, ext = NA_character_, extract.names = FALSE) {

	# Pattern
	pattern <- paste('^', db, '-.*', sep = '')
	if ( ! is.na(ext))
		pattern <- paste(pattern, ext, sep = '\\.')
	pattern <- paste(pattern, '$', sep = '')

	# List files
	dir <- .self$getFolderPath(folder)
	.self$message(MSG.DEBUG, paste("List files in", dir, "using pattern ", pattern))
	files <- list.files(path = dir, pattern = pattern)

	# Extract only the name part
	if (extract.names) {
		pattern <- paste('^', db, '-(.*)', sep = '')
		if ( ! is.na(ext))
			pattern <- paste(pattern, ext, sep = '\\.')
		pattern <- paste(pattern, '$', sep = '')
		files <- sub(pattern, '\\1', files, perl = TRUE)
	}

	return(files)
})

# DEPRECATED METHODS {{{1
################################################################

# Enabled {{{2
################################################################

BiodbCache$methods( enabled = function() {

	.self$.deprecated.method("BiodbConfig::isEnabled('cache.system')")

	return(.self$getBiodb()$getConfig()$isEnabled('cache.system'))
})

# Enable {{{2
################################################################

BiodbCache$methods( enable = function() {

	.self$.deprecated.method("BiodbConfig::enable('cache.system')")
	
	.self$getBiodb()$getConfig()$enable('cache.system')
})

# Disable {{{2
################################################################

BiodbCache$methods( disable = function() {

	.self$.deprecated.method("BiodbConfig::disable('cache.system')")
	
	.self$getBiodb()$getConfig()$disable('cache.system')
})
