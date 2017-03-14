# vi: fdm=marker

# Class declaration {{{1
################################################################

MirbaseMatureConn <- methods::setRefClass("MirbaseMatureConn", contains = "MirbaseConn")

# Constructor {{{1
################################################################

MirbaseMatureConn$methods( initialize = function(...) {
	callSuper(content.type = BIODB.TXT, ...)
})

# Get entry page url {{{1
################################################################

MirbaseMatureConn$methods( getEntryPageUrl = function(id) {
	return(paste(.self$getBaseUrl(), 'cgi-bin/mature.pl?mature_acc=', id, sep = ''))
})

# Download {{{1
################################################################

MirbaseMatureConn$methods( download = function() {

	if ( ! .self$getBiodb()$getCache()$markerExists(db = BIODB.MIRBASE.MATURE, folder = CACHE.LONG.TERM.FOLDER, name = 'extracted')) {

		# Download
		gz.path <- .self$getBiodb()$getCache()$getFilePaths(db = BIODB.MIRBASE.MATURE, folder = CACHE.LONG.TERM.FOLDER, names = 'download', ext = 'gz')
		if ( ! file.exists(gz.path)) {
			gz.url <- 'ftp://mirbase.org/pub/mirbase/CURRENT/mature.fa.gz'
			.self$message(MSG.INFO, paste("Downloading \"", gz.url, "\"...", sep = ''))
			.self$.getUrlScheduler()$downloadFile(url = gz.url, dest.file = gz.path)
		}

		# Extract
		# We do this because of the warning "seek on a gzfile connection returned an internal error" when using `gzfile()`.
		extracted.file <- tempfile(BIODB.MIRBASE.MATURE)
		R.utils::gunzip(filename = gz.path, destname = extracted.file, remove = FALSE)

		# Read file
		fd <- file(extracted.file, 'r')
		lines <- readLines(fd)
		close(fd)

		# Get all entry IDs
		ids <- sub('^.*(MIMAT[0-9]+).*$', '\\1', grep('MIMAT', lines, value = TRUE), perl = TRUE)
		.self$message(MSG.DEBUG, paste("Found ", length(ids), " entries in file \"", gz.path, "\".", sep = ''))

		if (length(ids) > 0) {
			# Get contents
			contents <- paste(lines[seq(1, length(ids), 2)], lines[seq(2, length(ids), 2)], sep = "\n")

			# Write all entries into files
			.self$getBiodb()$getCache()$deleteFiles(db = BIODB.MIRBASE.MATURE, folder = CACHE.SHORT.TERM.FOLDER, ext = .self$getEntryContentType())
			.self$getBiodb()$getCache()$saveContentToFile(contents, db = BIODB.MIRBASE.MATURE, folder = CACHE.SHORT.TERM.FOLDER, names = ids, ext = .self$getEntryContentType())
		}

		# Remove extract directory
		unlink(extracted.file)

		# Set marker
		.self$getBiodb()$getCache()$setMarker(db = BIODB.MIRBASE.MATURE, folder = CACHE.LONG.TERM.FOLDER, name = 'extracted')
	}
})

# Get entry ids {{{1
################################################################

MirbaseMatureConn$methods( getEntryIds = function(max.results = NA_integer_) {

	ids <- NULL

	# Download
	.self$download()

	# Get IDs from cache
	ids <- .self$getBiodb()$getCache()$listFiles(db = BIODB.MIRBASE.MATURE, folder = CACHE.SHORT.TERM.FOLDER, ext = .self$getEntryContentType(), extract.names = TRUE)

	# Filter out wrong IDs
	ids <- ids[grepl("^MIMAT[0-9]+$", ids, perl = TRUE)]

	# Cut
	if ( ! is.na(max.results) && max.results < length(ids))
		ids <- ids[1:max.results]

	return(ids)
})

# Get entry content {{{1
################################################################

MirbaseMatureConn$methods( getEntryContent = function(ids) {

	# Download
	.self$download()

	# Load content from cache
	content <- .self$getBiodb()$getCache()$loadFileContent(db = BIODB.MIRBASE.MATURE, folder = CACHE.SHORT.TERM.FOLDER, names = ids, ext = .self$getEntryContentType(), output.vector = TRUE)

	return(content)
})

# Find by name {{{1
################################################################

MirbaseMatureConn$methods( findByName = function(name) {

	# Get HTML
	htmlstr <- .self$.get.url('http://www.mirbase.org/cgi-bin/query.pl', params = c(terms = name, submit = 'Search'))

	# Parse HTML
	xml <-  XML::htmlTreeParse(htmlstr, asText = TRUE, useInternalNodes = TRUE)

	# Get accession number
	acc <- unlist(XML::xpathSApply(xml, "//a[starts-with(.,'MIMAT')]", XML::xmlValue))

	return(acc)
})
