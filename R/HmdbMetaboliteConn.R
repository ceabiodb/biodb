# vi: fdm=marker

# Class declaration {{{1
################################################################

HmdbMetaboliteConn <- methods::setRefClass("HmdbMetaboliteConn", contains = "RemotedbConn", fields = list(.ns = "character"))

# Constructor {{{1
################################################################

HmdbMetaboliteConn$methods( initialize = function(...) {

	callSuper(content.type = BIODB.XML, base.url = "http://www.hmdb.ca/", ...)

	# Set XML namespace
	.ns <<- c(hmdb = 'http://www.hmdb.ca')
})

# Get entry content {{{1
################################################################

HmdbMetaboliteConn$methods( getEntryContent = function(id) {

	# Initialize return values
	content <- rep(NA_character_, length(id))

	# Get URLs
	urls <- .self$getEntryContentUrl(id)

	# Request
	content <- vapply(urls, function(url) .self$.getUrlScheduler()$getUrl(url), FUN.VALUE = '')

	return(content)
})

# Create entry {{{1
################################################################

HmdbMetaboliteConn$methods( createEntry = function(content, drop = TRUE) {

	entries <- list()

	# Define xpath expressions
	xpath.expr <- character()
	xpath.expr[[BIODB.ACCESSION]]          <- "/metabolite/accession"
	xpath.expr[[BIODB.KEGG.COMPOUND.ID]]     <- "//kegg_id"
	xpath.expr[[BIODB.CHEBI.ID]]             <- "//chebi_id"
	xpath.expr[[BIODB.CHEMSPIDER.ID]]        <- "//chemspider_id"
	xpath.expr[[BIODB.NCBI.PUBCHEM.COMP.ID]] <- "//pubchem_compound_id"
	xpath.expr[[BIODB.NAME]]               <- "/metabolite/name"
	xpath.expr[[BIODB.FORMULA]]            <- "/metabolite/chemical_formula"
	xpath.expr[[BIODB.SUPER.CLASS]]        <- "//super_class"
	xpath.expr[[BIODB.AVERAGE.MASS]]       <- "//average_molecular_weight"
	xpath.expr[[BIODB.MONOISOTOPIC.MASS]]  <- "//monisotopic_moleculate_weight"
	xpath.expr[[BIODB.SYNONYMS]]           <- "//synonym"
	xpath.expr[[BIODB.COMP.IUPAC.NAME.SYST]]           <- "//iupac_name"
	xpath.expr[[BIODB.COMP.IUPAC.NAME.TRAD]]           <- "//traditional_iupac"
	xpath.expr[[BIODB.CAS.ID]]           <- "//cas_registry_number"
	xpath.expr[[BIODB.SMILES]]           <- "//smiles"
	xpath.expr[[BIODB.INCHI]]           <- "//inchi"
	xpath.expr[[BIODB.INCHIKEY]]           <- "//inchikey"

	for (single.content in content) {

		# Create instance
		entry <- BiodbEntry$new(.self$getBiodb())

		if ( ! is.null(single.content) && ! is.na(single.content)) {

			# Parse XML
			xml <-  XML::xmlInternalTreeParse(single.content, asText = TRUE)

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

		# Correct InChIKey
		if (entry$hasField(BIODB.INCHIKEY))
			entry$setFieldValue(BIODB.INCHIKEY, sub('^InChIKey=', '', entry$getFieldValue(BIODB.INCHIKEY), perl = TRUE))

		entries <- c(entries, entry)
	}

	# Replace elements with no accession id by NULL
	entries <- lapply(entries, function(x) if (is.na(x$getField(BIODB.ACCESSION))) NULL else x)

	# If the input was a single element, then output a single object
	if (drop && length(content) == 1)
		entries <- entries[[1]]

	return(entries)
})

# Download {{{1
################################################################

HmdbMetaboliteConn$methods( download = function() {

	if ( ! .self$getBiodb()$getCache()$fileExists(BIODB.HMDB.METABOLITE, 'download', 'zip')) {

		# Download
		zip.path <- .self$getBiodb()$getCache()$getFilePaths(BIODB.HMDB.METABOLITE, 'download', 'zip')
		zip.url <- paste(.self$getBaseUrl(), "system/downloads/current/hmdb_metabolites.zip", sep = '')
		.self$message(MSG.INFO, paste("Downloading \"", zip.url, "\"...", sep = ''))
		utils::download.file(url = zip.url, destfile = zip.path, method = 'libcurl', cacheOK = FALSE, quiet = TRUE)

		# Expand zip
		extract.dir <- tempfile(BIODB.HMDB.METABOLITE)
		utils::unzip(zip.path, exdir = extract.dir)
		
		# Load extracted XML file
		files <- list.files(path = extract.dir)
		xml.file <- NULL
		if (length(files) == 0)
			.self$message(MSG.ERROR, paste("No XML file found in zip file \"", zip.path, "\".", sep = ''))
		else if (length(files) == 1)
			xml.file <- file.path(extract.dir, files)
		else {
			for (f in c('hmdb_metabolites.xml', 'hmdb_metabolites_tmp.xml'))
				if (f %in% files)
					xml.file <- file.path(extract.dir, f)
			if (is.null(xml.file))
				.self$message(MSG.ERROR, paste("More than one file found in zip file \"", zip.path, "\":", paste(files, collapse = ", "), ".", sep = ''))
		}
		xml <- XML::xmlInternalTreeParse(xml.file)

		# Write all XML entries into files
		ids <- XML::xpathSApply(xml, "//hmdb:metabolite/hmdb:accession", XML::xmlValue, namespaces = .self$.ns)
		.self$message(MSG.DEBUG, paste("Found ", length(ids), " entries in file \"", xml.file, "\".", sep = ''))
		contents <- vapply(XML::getNodeSet(xml, "//hmdb:metabolite", namespaces = .self$.ns), XML::saveXML, FUN.VALUE = '')
		.self$getBiodb()$getCache()$deleteFiles(BIODB.HMDB.METABOLITE, .self$getEntryContentType())
		.self$getBiodb()$getCache()$saveContentToFile(contents, BIODB.HMDB.METABOLITE, ids, .self$getEntryContentType())

		# Remove extract directory
		unlink(extract.dir, recursive = TRUE)
	}
})

# Get entry ids {{{1
################################################################

HmdbMetaboliteConn$methods( getEntryIds = function(max.results = NA_integer_) {

	ids <- NULL

	# Do we allow database download? This can take some time.
	if (.self$getBiodb()$getConfig()$isEnabled(CFG.ALLOW.HUGE.DOWNLOADS)) {

		# Download
		.self$download()

		# Get IDs from cache
		ids <- .self$getBiodb()$getCache()$listFiles(BIODB.HMDB.METABOLITE, .self$getEntryContentType(), extract.names = TRUE)

		# Filter out wrong IDs
		ids <- ids[grepl("^HMDB[0-9]+$", ids, perl = TRUE)]

		# Cut
		if ( ! is.na(max.results) && max.results < length(ids))
			ids <- ids[1:max.results]
	}

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
