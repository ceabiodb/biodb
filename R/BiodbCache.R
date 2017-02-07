# vi: fdm=marker

# Class declaration {{{1
################################################################

#'A class for constructing biodb objects.
#'@export
BiodbCache <- methods::setRefClass("BiodbCache", contains = 'BiodbObject', fields = list( .mode = "character", .biodb = "ANY", .enabled = "logical"))

# Constants {{{1
################################################################

#'@export
CACHE.READ.ONLY  <- 'read-only'

#'@export
CACHE.READ.WRITE <- 'read-write'

#'@export
CACHE.WRITE.ONLY <- 'write-only'

# Constructor {{{1
################################################################

BiodbCache$methods( initialize = function(biodb = NULL, ...) {

	.enabled <<- TRUE
	.biodb <<- biodb
	.mode <<- CACHE.READ.WRITE

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

	cachedir <- .self$getBiodb()$getConfig()$get(CFG.CACHEDIR)

	# Create cache dir if needed
	if ( ! is.na(cachedir) && ! file.exists(cachedir))
		dir.create(cachedir)

	return(cachedir)
})

# Set mode {{{1
################################################################

BiodbCache$methods( setMode = function(mode) {

	# Check mode		   
	if ( ! mode %in% c(CACHE.READ.ONLY, CACHE.READ.WRITE, CACHE.WRITE.ONLY))
		.self$message(MSG.ERROR, paste0("Invalid value \"", mode, "\" for cache mode."))

	# Set mode
	.mode <<- mode
})

# Is cache reading enabled {{{1
################################################################

BiodbCache$methods( isReadable = function() {
	return( .self$enabled() && ! is.na(.self$getDir()) && .self$.mode %in% c(CACHE.READ.ONLY, CACHE.READ.WRITE))
})

# Is cache writing enabled {{{1
################################################################

BiodbCache$methods( isWritable = function() {
	return( .self$enabled() && ! is.na(.self$getDir()) && .self$.mode %in% c(CACHE.WRITE.ONLY, CACHE.READ.WRITE))
})

# File exists {{{1
################################################################

BiodbCache$methods( fileExists = function(db, names, ext) {

	return(file.exists(.self$getFilePaths(db, names, ext)))
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

BiodbCache$methods( loadFileContent = function(db, names, ext) {

	content <- NULL

	# Read contents from files
	file.paths <- .self$getBiodb()$getCache()$getFilePaths(db, names, ext)
	content <- lapply(file.paths, function(x) { if (is.na(x)) NA_character_ else ( if (file.exists(x)) paste(readLines(x), collapse = "\n") else NULL )} )

	return(content)
})

# Save content into file {{{1
################################################################

BiodbCache$methods( saveContentToFile = function(contents, db, names, ext) {

	# Write contents into files
	file.paths <- .self$getBiodb()$getCache()$getFilePaths(db, names, ext)
	mapply(function(c, f) { if ( ! is.null(c)) writeLines(c, f) }, contents, file.paths)
})

# Delete files {{{1
################################################################

BiodbCache$methods( deleteFiles = function(db, ext = NA_character_) {

	files <- paste(db, '*',sep = '-')
	if ( ! is.na(ext))
		files <- paste(db, ext, sep = '.')

	unlink(file.path(.self$getDir(), files))
})
