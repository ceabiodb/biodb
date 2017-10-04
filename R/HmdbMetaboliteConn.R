# vi: fdm=marker

#' @include RemotedbConn.R
#' @include BiodbDownloadable.R

# Class declaration {{{1
################################################################

HmdbMetaboliteConn <- methods::setRefClass("HmdbMetaboliteConn", contains = c("RemotedbConn", 'BiodbDownloadable'), fields = list(.ns = "character"))

# Constructor {{{1
################################################################

HmdbMetaboliteConn$methods( initialize = function(...) {

	callSuper(content.type = BIODB.XML, base.url = "http://www.hmdb.ca/", ...)

	# Set XML namespace
	.ns <<- c(hmdb = 'http://www.hmdb.ca')

	.self$.setDownloadExt('zip')
})

# Get entry content {{{1
################################################################

HmdbMetaboliteConn$methods( getEntryContent = function(entry.id) {

	# Initialize return values
	content <- rep(NA_character_, length(entry.id))

	# Get URLs
	urls <- .self$getEntryContentUrl(entry.id)

	# Request
	content <- vapply(urls, function(url) .self$.getUrlScheduler()$getUrl(url), FUN.VALUE = '')

	return(content)
})


# Do download {{{1
################################################################

HmdbMetaboliteConn$methods( .doDownload = function() {

	# Download
	.self$message('info', "Downloading HMDB metabolite database...")
	zip.url <- paste(.self$getBaseUrl(), "system/downloads/current/hmdb_metabolites.zip", sep = '')
	.self$message('info', paste("Downloading \"", zip.url, "\"...", sep = ''))
	.self$.getUrlScheduler()$downloadFile(url = zip.url, dest.file = .self$getDownloadPath())
})

# Do extract download {{{1
################################################################

HmdbMetaboliteConn$methods( .doExtractDownload = function() {

	.self$message('info', "Extracting content of downloaded HMDB metabolite database...")

	# Expand zip
	extract.dir <- tempfile(.self$getId())
	utils::unzip(.self$getDownloadPath(), exdir = extract.dir)
	
	# Load extracted XML file
	files <- list.files(path = extract.dir)
	xml.file <- NULL
	if (length(files) == 0)
		.self$message('error', paste("No XML file found in zip file \"", .self$getDownloadPath(), "\".", sep = ''))
	else if (length(files) == 1)
		xml.file <- file.path(extract.dir, files)
	else {
		for (f in c('hmdb_metabolites.xml', 'hmdb_metabolites_tmp.xml'))
			if (f %in% files)
				xml.file <- file.path(extract.dir, f)
		if (is.null(xml.file))
			.self$message('error', paste("More than one file found in zip file \"", .self$getDownloadPath(), "\":", paste(files, collapse = ", "), ".", sep = ''))
	}
	xml <- XML::xmlInternalTreeParse(xml.file)

	# Get IDs of all entries
	ids <- XML::xpathSApply(xml, "//hmdb:metabolite/hmdb:accession", XML::xmlValue, namespaces = .self$.ns)
	.self$message('debug', paste("Found ", length(ids), " entries in file \"", xml.file, "\".", sep = ''))

	# Get contents of all entries
	contents <- vapply(XML::getNodeSet(xml, "//hmdb:metabolite", namespaces = .self$.ns), XML::saveXML, FUN.VALUE = '')
	Encoding(contents) <- "unknown" # Force encoding to 'unknown', since leaving it set to 'UTF-8' will produce outputs of the form '<U+2022>' for unicode characters. These tags '<U+XXXX>' will then be misinterpreted by XML parser when reading entry content, because there is no slash at the end of the tag.

	# Delete existing cache files
	.self$getBiodb()$getCache()$deleteFiles(dbid = .self$getId(), subfolder = 'shortterm', ext = .self$getEntryContentType())

	# Write all XML entries into files
	.self$getBiodb()$getCache()$saveContentToFile(contents, dbid = .self$getId(), subfolder = 'shortterm', name = ids, ext = .self$getEntryContentType())

	# Remove extract directory
	unlink(extract.dir, recursive = TRUE)
})

# Get entry ids {{{1
################################################################

HmdbMetaboliteConn$methods( getEntryIds = function(max.results = NA_integer_) {

	ids <- NULL

	# Download
	.self$download()

	# Get IDs from cache
	ids <- .self$getBiodb()$getCache()$listFiles(dbid = .self$getId(), subfolder = 'shortterm', ext = .self$getEntryContentType(), extract.name = TRUE)

	# Filter out wrong IDs
	ids <- ids[grepl("^HMDB[0-9]+$", ids, perl = TRUE)]

	# Cut
	if ( ! is.na(max.results) && max.results < length(ids))
		ids <- ids[1:max.results]

	return(ids)
})

# Get nb entries {{{1
################################################################

HmdbMetaboliteConn$methods( getNbEntries = function(count = FALSE) {

	n <- NA_integer_

	ids <- .self$getEntryIds()
	if ( ! is.null(ids))
		n <- length(ids)

	return(n)
})

# Do get entry content url {{{1
################################################################

HmdbMetaboliteConn$methods( .doGetEntryContentUrl = function(id, concatenate = TRUE) {

	url <- paste0(.self$getBaseUrl(), 'metabolites/', id, '.xml')

	return(url)
})

# Get entry page url {{{1
################################################################

HmdbMetaboliteConn$methods( getEntryPageUrl = function(id) {

	url <- paste0(.self$getBaseUrl(), 'metabolites/', id)

	return(url)
})
