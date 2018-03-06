# vi: fdm=marker

#' @include MirbaseConn.R
#' @include BiodbDownloadable.R

# Class declaration {{{1
################################################################

MirbaseMatureConn <- methods::setRefClass("MirbaseMatureConn", contains = c("MirbaseConn", "BiodbDownloadable"))

# Constructor {{{1
################################################################

MirbaseMatureConn$methods( initialize = function(...) {
	callSuper(...)

	.self$.setDownloadExt('gz')
})

# Get entry page url {{{1
################################################################

MirbaseMatureConn$methods( getEntryPageUrl = function(id) {
	return(paste(.self$getBaseUrl(), 'cgi-bin/mature.pl?mature_acc=', id, sep = ''))
})

# Get entry image url {{{1
################################################################

MirbaseMatureConn$methods( getEntryImageUrl = function(id) {
	return(rep(NA_character_, length(id)))
})

# Requires download {{{1
################################################################

MirbaseMatureConn$methods( requiresDownload = function() {
	return(TRUE)
})

# Do download {{{1
################################################################

MirbaseMatureConn$methods( .doDownload = function() {

	# Download
	gz.url <- 'ftp://mirbase.org/pub/mirbase/CURRENT/mature.fa.gz'
	.self$message('info', paste("Downloading \"", gz.url, "\"...", sep = ''))
	.self$.getUrlScheduler()$downloadFile(url = gz.url, dest.file = .self$getDownloadPath())
})

# Do extract download {{{1
################################################################

MirbaseMatureConn$methods( .doExtractDownload = function() {

	# Extract
	# We do this because of the warning "seek on a gzfile connection returned an internal error" when using `gzfile()`.
	extracted.file <- tempfile(.self$getId())
	R.utils::gunzip(filename = .self$getDownloadPath(), destname = extracted.file, remove = FALSE)

	# Read file
	fd <- file(extracted.file, 'r')
	lines <- readLines(fd)
	close(fd)

	# Get all entry IDs
	ids <- sub('^.*(MIMAT[0-9]+).*$', '\\1', grep('MIMAT', lines, value = TRUE), perl = TRUE)
	.self$message('debug', paste("Found ", length(ids), " entries in file \"", .self$getDownloadPath(), "\".", sep = ''))

	if (length(ids) > 0) {
		# Get contents
		contents <- paste(lines[seq(1, 2*length(ids), 2)], lines[seq(2, 2*length(ids), 2)], sep = "\n")

		# Write all entries into files
		.self$getBiodb()$getCache()$deleteFiles(dbid = .self$getId(), subfolder = 'shortterm', ext = .self$getEntryContentType())
		.self$getBiodb()$getCache()$saveContentToFile(contents, dbid = .self$getId(), subfolder = 'shortterm', name = ids, ext = .self$getEntryContentType())
	}

	# Remove extract directory
	unlink(extracted.file)
})

# Get entry ids {{{1
################################################################

MirbaseMatureConn$methods( getEntryIds = function(max.results = NA_integer_) {

	ids <- NULL

	# Download
	.self$download()

	# Get IDs from cache
	ids <- .self$getBiodb()$getCache()$listFiles(dbid = .self$getId(), subfolder = 'shortterm', ext = .self$getEntryContentType(), extract.name = TRUE)

	# Filter out wrong IDs
	ids <- ids[grepl("^MIMAT[0-9]+$", ids, perl = TRUE)]

	# Cut
	if ( ! is.na(max.results) && max.results < length(ids))
		ids <- ids[1:max.results]

	return(ids)
})

# Get entry content {{{1
################################################################

MirbaseMatureConn$methods( getEntryContent = function(entry.id) {

	# Download
	.self$download()

	# Load content from cache
	content <- .self$getBiodb()$getCache()$loadFileContent(dbid = .self$getId(), subfolder = 'shortterm', name = entry.id, ext = .self$getEntryContentType(), output.vector = TRUE)

	return(content)
})

# Web service query {{{1
################################################################

MirbaseMatureConn$methods( ws.query = function(terms, submit = 'Search', biodb.parse = FALSE, biodb.ids = FALSE) {

	# Build request
	url <- paste0(.self$getBaseUrl(), 'cgi-bin/query.pl')
	params <- c(terms = terms, submit = submit)

	# Send request
	results <- .self$.getUrlScheduler()$getUrl(url, params = params)

	# Parse HTML
	if (biodb.parse || biodb.ids)
		results <-  XML::htmlTreeParse(results, asText = TRUE, useInternalNodes = TRUE)

	# Get IDs
	if (biodb.ids) {
		results <- unlist(XML::xpathSApply(results, "//a[starts-with(.,'MIMAT')]", XML::xmlValue))
		if (is.null(results))
			results <- character()
	}

	return(results)
})

# Search compound {{{1
################################################################

MirbaseMatureConn$methods( searchCompound = function(name = NULL, mass = NULL, mass.field = NULL, mass.tol = 0.01, mass.tol.unit = 'plain', max.results = NA_integer_) {

	ids <- NULL

	# Search by name
	if ( ! is.null(name))
		ids <- .self$ws.query(terms = name, biodb.ids = TRUE)

	# Search by mass
	if ( ! is.null(mass.field))
		.self$message('caution', 'Searching by mass is not possible.')

	return(ids)
})
