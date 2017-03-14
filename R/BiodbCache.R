# vi: fdm=marker

#' @include ChildObject.R

# Constants {{{1
################################################################

CACHE.SHORT.TERM.FOLDER <- 'shortterm'
CACHE.LONG.TERM.FOLDER <- 'longterm'
CACHE.FOLDERS <- c(CACHE.SHORT.TERM.FOLDER, CACHE.LONG.TERM.FOLDER)

# Class declaration {{{1
################################################################

#'A class for constructing biodb objects.
#'@export
BiodbCache <- methods::setRefClass("BiodbCache", contains = 'ChildObject', fields = list(.enabled = "logical"))

# Constructor {{{1
################################################################

BiodbCache$methods( initialize = function(...) {

	callSuper(...)

	.enabled <<- TRUE
})

# Enabled {{{1
################################################################

BiodbCache$methods( enabled = function() {
	return(.self$.enabled)
})

# Enable {{{1
################################################################

BiodbCache$methods( enable = function() {
	.enabled <<- TRUE
})

# Disable {{{1
################################################################

BiodbCache$methods( disable = function() {
	.enabled <<- FALSE
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
	return( .self$enabled() && ! is.na(.self$getDir()))
})

# Is cache writing enabled {{{1
################################################################

BiodbCache$methods( isWritable = function() {
	return( .self$enabled() && ! is.na(.self$getDir()) && ! .self$getBiodb()$getConfig()$get(CFG.CACHE.READ.ONLY))
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
	content <- lapply(file.paths, function(x) { if (is.na(x)) NA_character_ else ( if (file.exists(x)) readChar(x, file.info(x)$size) else NULL )} )

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

	# Write contents into files
	file.paths <- .self$getBiodb()$getCache()$getFilePaths(db, folder, names, ext)
	if (length(file.paths) != length(contents))
		.self$message(MSG.ERROR, paste("The number of contents to save (", length(contents), ") is different from the number of paths (", length(file.paths), ").", sep = ''))
	mapply(function(c, f) { if ( ! is.null(c)) writeChar(if (is.na(c)) 'NA' else c, f) }, contents, file.paths)
})

# Get folder path {{{1
################################################################

BiodbCache$methods( getFolderPath = function(folder) {

	# Check folder
	if ( ! folder %in% CACHE.FOLDERS)
		.self$message(MSG.ERROR, paste("Unknown cache folder \"", folder, "\".", sep = ''))

	# Get folder path
	folder.path <- file.path(.self$getDir(), folder)

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
