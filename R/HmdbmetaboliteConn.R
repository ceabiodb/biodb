# vi: fdm=marker

# Class declaration {{{1
################################################################

HmdbmetaboliteConn <- methods::setRefClass("HmdbmetaboliteConn", contains = "RemotedbConn", fields = list(.ns = "character"))

# Constructor {{{1
################################################################

HmdbmetaboliteConn$methods( initialize = function(...) {

	callSuper(base.url = "http://www.hmdb.ca/", ...)

	# Set XML namespace
	.ns <<- c(hmdb = 'http://www.hmdb.ca')
})

# Get entry content type {{{1
################################################################

HmdbmetaboliteConn$methods( getEntryContentType = function() {
	return(BIODB.XML)
})

# Get entry content {{{1
################################################################

HmdbmetaboliteConn$methods( getEntryContent = function(id) {

	# Initialize return values
	content <- rep(NA_character_, length(id))

	# Request
	content <- vapply(id, function(x) .self$.get.url(get.entry.url(BIODB.HMDBMETABOLITE, x, content.type = BIODB.XML)), FUN.VALUE = '')

	return(content)
})

# Create entry {{{1
################################################################

HmdbmetaboliteConn$methods( createEntry = function(contents, drop = TRUE) {

	entries <- list()

	# Define xpath expressions
	xpath.expr <- character()
	xpath.expr[[BIODB.ACCESSION]]          <- "/metabolite/accession"
	xpath.expr[[BIODB.KEGGCOMPOUND.ID]]    <- "//kegg_id"
	xpath.expr[[BIODB.NAME]]               <- "/metabolite/name"
	xpath.expr[[BIODB.FORMULA]]            <- "/metabolite/chemical_formula"
	xpath.expr[[BIODB.SUPER.CLASS]]        <- "//super_class"
	xpath.expr[[BIODB.AVERAGE.MASS]]       <- "//average_molecular_weight"
	xpath.expr[[BIODB.MONOISOTOPIC.MASS]]  <- "//monisotopic_moleculate_weight"

	for (content in contents) {

		# Create instance
		entry <- BiodbEntry$new(.self$getBiodb())

		if ( ! is.null(content) && ! is.na(content)) {

			# Parse XML
			xml <-  XML::xmlInternalTreeParse(content, asText = TRUE)

			# An error occured
			if (length(XML::getNodeSet(xml, "//error")) == 0) {

				# Test generic xpath expressions
				for (field in names(xpath.expr)) {
					v <- XML::xpathSApply(xml, xpath.expr[[field]], XML::xmlValue)
					if (length(v) > 0)
						entry$setField(field, v)
				}

			}
		}

		entries <- c(entries, entry)
	}

	# Replace elements with no accession id by NULL
	entries <- lapply(entries, function(x) if (is.na(x$getField(BIODB.ACCESSION))) NULL else x)

	# If the input was a single element, then output a single object
	if (drop && length(contents) == 1)
		entries <- entries[[1]]

	return(entries)
})

# Download {{{1
################################################################

HmdbmetaboliteConn$methods( download = function() {

	if ( ! .self$getBiodb()$getCache()$fileExists(BIODB.HMDBMETABOLITE, 'download', 'zip')) {

		# Download
		zip.path <- .self$getBiodb()$getCache()$getFilePaths(BIODB.HMDBMETABOLITE, 'download', 'zip')
		zip.url <- paste(.self$getBaseUrl(), "system/downloads/current/hmdb_metabolites.zip", sep = '')
		.self$message(MSG.INFO, paste("Downloading \"", zip.url, "\"...", sep = ''))
		download.file(url = zip.url, destfile = zip.path, method = 'libcurl', cacheOK = FALSE, quiet = TRUE)

		# Expand zip
		extract.dir <- tempfile(BIODB.HMDBMETABOLITE)
		unzip(zip.path, exdir = extract.dir)
		
		# Load extracted XML file
		xml.file <- file.path(extract.dir, list.files(path = extract.dir))
		if (length(xml.file) == 0)
			.self$message(MSG.ERROR, paste("No XML file found in zip file \"", zip.path, "\".", sep = ''))
		if (length(xml.file) > 1)
			.self$message(MSG.ERROR, paste("More than one file found in zip file \"", zip.path, "\".", sep = ''))
		xml <- XML::xmlInternalTreeParse(xml.file)

		# Write all XML entries into files
		ids <- XML::xpathSApply(xml, "//hmdb:metabolite/hmdb:accession", XML::xmlValue, namespaces = .self$.ns)
		.self$message(MSG.DEBUG, paste("Found ", length(ids), " entries in file \"", xml.file, "\".", sep = ''))
		contents <- vapply(XML::getNodeSet(xml, "//hmdb:metabolite", namespaces = .self$.ns), XML::saveXML, FUN.VALUE = '')
		.self$getBiodb()$getCache()$deleteFiles(BIODB.HMDBMETABOLITE, .self$getEntryContentType())
		.self$getBiodb()$getCache()$saveContentToFile(contents, BIODB.HMDBMETABOLITE, ids, .self$getEntryContentType())
	}
})

# Get entry ids {{{1
################################################################

HmdbmetaboliteConn$methods( getEntryIds = function(max.results = NA_integer_) {

	ids <- NULL

	# Do we allow database download? This can take some time.
	if (.self$getBiodb()$getConfig()$isEnabled(CFG.DBDWNLD)) {

		# Download
		.self$download()


	}

	return(ids)
})
