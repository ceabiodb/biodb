# vi: fdm=marker

# Class declaration {{{1
################################################################

#' @include RemotedbConn.R
#' @include CompounddbConn.R
#' @include BiodbDownloadable.R
HmdbMetabolitesConn <- methods::setRefClass("HmdbMetabolitesConn", contains = c("RemotedbConn", "CompounddbConn", 'BiodbDownloadable'), fields = list(.ns = "character"))

# Constructor {{{1
################################################################

HmdbMetabolitesConn$methods( initialize = function(...) {

	callSuper(...)

	.self$.setDownloadExt('zip')
})

# Get entry content {{{1
################################################################

HmdbMetabolitesConn$methods( getEntryContent = function(entry.id) {

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

HmdbMetabolitesConn$methods( .doDownload = function() {

	# Download
	.self$message('info', "Downloading HMDB metabolite database...")
	zip.url <- paste(.self$getBaseUrl(), "system/downloads/current/hmdb_metabolites.zip", sep = '')
	.self$message('info', paste("Downloading \"", zip.url, "\"...", sep = ''))
	.self$.getUrlScheduler()$downloadFile(url = zip.url, dest.file = .self$getDownloadPath())
})

# Do extract download {{{1
################################################################

HmdbMetabolitesConn$methods( .doExtractDownload = function() {

	.self$message('info', "Extracting content of downloaded HMDB metabolite database...")

	# Expand zip
	extract.dir <- tempfile(.self$getId())
	zip.path <- .self$getDownloadPath()
	.self$message('debug', paste("Unzipping ", zip.path, "...", sep = ''))
	utils::unzip(zip.path, exdir = extract.dir)
	.self$message('debug', paste("Unzipped ", zip.path, ".", sep  = ''))
	
	# Search for extracted XML file
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
	if (is.null(xml.file))
		.self$message('error', "No XML file found in ZIP file.")
	.self$message('debug', paste("Found XML file ", xml.file, " in ZIP file.", sep  = ''))

	if (TRUE) {
		# Split XML file
		.self$message('debug', "Load XML file.")
		xml <- readChar(xml.file, file.info(xml.file)$size, useBytes = TRUE)
		.self$message('debug', paste("XML string has", nchar(xml), "characters."))
		.self$message('debug', paste("XML string first 100 characters are", substr(xml, 1, 100), ".", sep = '"'))
		.self$message('debug', "Split XML string on <metabolite> tags.")
		contents <- strsplit(xml, '<metabolite>')[[1]]
		.self$message('debug', "Remove first element.")
		contents <- contents[2:length(contents)] # Remove first one (XML header and <hmdb> tag, not a metabolite
		.self$message('debug', "Put back <metabolite> tag.")
		contents <- vapply(contents, function(s) paste0('<metabolite>', s), FUN.VALUE = '') # Put back <metabolite> tag.
		.self$message('debug', "Remove last </hmdb> tag.")
		contents[length(contents)] <- sub('</hmdb>', '', contents[length(contents)]) # Remove HMDB end tag.
		.self$message('debug', 'Get IDs of entries.')
		ids <- stringr::str_match(contents, '<accession>(HMDB[0-9]+)</accession>')[, 2]
	} else {
		# Load extracted XML file
		xml <- XML::xmlInternalTreeParse(xml.file)

		# Get IDs of all entries
		ids <- XML::xpathSApply(xml, "//hmdb:metabolite/hmdb:accession", XML::xmlValue, namespaces = c(hmdb = .self$getDbInfo()$getXmlNs()))
		.self$message('debug', paste("Found ", length(ids), " entries in file \"", xml.file, "\".", sep = ''))

		# Get contents of all entries
		contents <- vapply(XML::getNodeSet(xml, "//hmdb:metabolite", namespaces = c(hmdb = .self$getDbInfo()$getXmlNs())), XML::saveXML, FUN.VALUE = '')
		Encoding(contents) <- "unknown" # Force encoding to 'unknown', since leaving it set to 'UTF-8' will produce outputs of the form '<U+2022>' for unicode characters. These tags '<U+XXXX>' will then be misinterpreted by XML parser when reading entry content, because there is no slash at the end of the tag.
	}

	# Delete existing cache files
	.self$message('debug', 'Delete existing entry files in cache system.')
	.self$getBiodb()$getCache()$deleteFiles(dbid = .self$getId(), subfolder = 'shortterm', ext = .self$getEntryContentType())

	# Write all XML entries into files
	.self$message('debug', 'Write all entries into files in cache system.')
	.self$getBiodb()$getCache()$saveContentToFile(contents, dbid = .self$getId(), subfolder = 'shortterm', name = ids, ext = .self$getEntryContentType())

	# Remove extract directory
	.self$message('debug', 'Delete extract directory.')
	unlink(extract.dir, recursive = TRUE)
})

# Get entry ids {{{1
################################################################

HmdbMetabolitesConn$methods( getEntryIds = function(max.results = NA_integer_) {

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

HmdbMetabolitesConn$methods( getNbEntries = function(count = FALSE) {

	n <- NA_integer_

	ids <- .self$getEntryIds()
	if ( ! is.null(ids))
		n <- length(ids)

	return(n)
})

# Do get entry content url {{{1
################################################################

HmdbMetabolitesConn$methods( .doGetEntryContentUrl = function(id, concatenate = TRUE) {

	url <- paste0(.self$getBaseUrl(), 'metabolites/', id, '.xml')

	return(url)
})

# Get entry page url {{{1
################################################################

HmdbMetabolitesConn$methods( getEntryPageUrl = function(id) {
	return(paste0(.self$getBaseUrl(), 'metabolites/', id))
})

# Get entry image url {{{1
################################################################

HmdbMetabolitesConn$methods( getEntryImageUrl = function(id) {
	return(paste0(.self$getBaseUrl(), 'structures/', id, '/image.png'))
})

# Search compound {{{1
################################################################

HmdbMetabolitesConn$methods( searchCompound = function(name = NULL, molecular.mass = NULL, monoisotopic.mass = NULL, mass.tol = 0.01, mass.tol.unit = 'plain', max.results = NA_integer_) {

	ids <- NULL

	.self$message('caution', 'HMDB is not searchable. HMDB only provides an HTML interface for searching, giving results split across several pages. It is unpractical to use from a program. Since HMDB is downloaded entirely, a solution using an internal database will be implemented in the future.')

	return(ids)
})
