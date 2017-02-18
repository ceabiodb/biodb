# vi: fdm=marker

# Class declaration {{{1
################################################################

#'A class for constructing biodb objects.
#'@export
BiodbCache <- methods::setRefClass("BiodbCache", contains = 'BiodbObject', fields = list( .biodb = "ANY", .enabled = "logical", .read.only = "logical", .force.download = "logical"))

# Constructor {{{1
################################################################

BiodbCache$methods( initialize = function(biodb = NULL, ...) {

	.enabled <<- TRUE
	.biodb <<- biodb

	callSuper(...)
})

# Get biodb {{{1
################################################################

BiodbCache$methods( getBiodb = function() {
	return(.self$.biodb)
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

BiodbCache$methods( fileExists = function(db, names, ext) {

	exists <- FALSE

	if ( ! .self$getBiodb()$getConfig()$get(CFG.CACHE.FORCE.DOWNLOAD))
		exists <- file.exists(.self$getFilePaths(db, names, ext))

	return(exists)
})

# Get file paths {{{1
################################################################

BiodbCache$methods( getFilePaths = function(db, names, ext) {

	# set file paths
	filepaths <- file.path(.self$getDir(), paste(db, '-', names, '.', ext, sep = ''))

	# Set NA values
	filepaths[is.na(names)] <- NA_character_

	return(filepaths)
})

# Load file content {{{1
################################################################

BiodbCache$methods( loadFileContent = function(db, names, ext, output.vector = FALSE) {

	if ( ! .self$isReadable())
		.self$message(MSG.ERROR, paste("Attempt to read from non-readable cache \"", .self$getDir(), "\".", sep = ''))

	content <- NULL

	# Read contents from files
	file.paths <- .self$getBiodb()$getCache()$getFilePaths(db, names, ext)
	content <- lapply(file.paths, function(x) { if (is.na(x)) NA_character_ else ( if (file.exists(x)) paste(readLines(x), collapse = "\n") else NULL )} )

	# Set to NA
	content[content == 'NA'] <- NA_character_

	# Vector ?
	if (output.vector)
		content <- vapply(content, function(x) if (is.null(x)) NA_character_ else x, FUN.VALUE = '')

	return(content)
})

# Save content into file {{{1
################################################################

BiodbCache$methods( saveContentToFile = function(contents, db, names, ext) {

	if ( ! .self$isWritable())
		.self$message(MSG.ERROR, paste("Attempt to write into non-writable cache. \"", .self$getDir(), "\".", sep = ''))

	# Write contents into files
	file.paths <- .self$getBiodb()$getCache()$getFilePaths(db, names, ext)
	mapply(function(c, f) { if ( ! is.null(c)) writeLines(c, f) }, contents, file.paths)
})

# Delete files {{{1
################################################################

BiodbCache$methods( deleteFiles = function(db, ext = NA_character_) {

	files <- paste(db, '*',sep = '-')
	if ( ! is.na(ext))
		files <- paste(files, ext, sep = '.')

	unlink(file.path(.self$getDir(), files))
})

# List files {{{1
################################################################

BiodbCache$methods( listFiles = function(db, ext = NA_character_, extract.names = FALSE) {

	# Pattern
	pattern <- paste('^', db, '-.*', sep = '')
	if ( ! is.na(ext))
		pattern <- paste(pattern, ext, sep = '\\.')
	pattern <- paste(pattern, '$', sep = '')

	# List files
	.self$message(MSG.DEBUG, paste("List files in", .self$getDir(), "using pattern ", pattern))
	files <- list.files(path = .self$getDir(), pattern = pattern)

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
