# vi: fdm=marker

# Constants {{{1
################################################################

.BIODB.HMDB.METABOLITES.PARSING.EXPR <- list(
	'accession'             = "/metabolite/accession",
	'kegg.compound.id'      = "//kegg_id",
	'chebi.id'              = "//chebi_id",
	'chemspider.id'         = "//chemspider_id",
	'ncbi.pubchem.comp.id'  = "//pubchem_compound_id",
	'name'                  = "/metabolite/name",
	'formula'               = "/metabolite/chemical_formula",
	'super.class'           = "//super_class",
	'average.mass'          = "//average_molecular_weight",
	'monoisotopic.mass'     = "//monisotopic_molecular_weight",
	'comp.iupac.name.syst'  = "//iupac_name",
	'comp.iupac.name.trad'  = "//traditional_iupac",
	'cas.id'                = "//cas_registry_number",
	'smiles'                = "//smiles",
	'inchi'                 = "//inchi",
	'inchikey'              = "//inchikey"
)

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

	# Delete existing cache files
	.self$message('debug', 'Delete existing entry files in cache system.')
	.self$getBiodb()$getCache()$deleteFiles(.self$getCacheId(), subfolder = 'shortterm', ext = .self$getEntryContentType())

	# Open file
	file.conn <- file(xml.file, open = 'r')

	# Extract entries from XML file
	.self$message('debug', "Extract entries from XML file.")
	chunk.size <- 2**16
	total.bytes <- file.info(xml.file)$size
	bytes.read <- 0
	last.msg.bytes.index <- 0
	xml.chunks <- character()
	.self$message('debug', paste("Read XML file by chunk of", chunk.size, "characters."))
	done.reading <- FALSE
	while ( ! done.reading) {

		# Read chunk from file
		chunk <- readChar(file.conn, chunk.size)
		done.reading <- (nchar(chunk) < chunk.size)

		# Send progress message
		bytes.read <- bytes.read + nchar(chunk, type = 'bytes')
		if (bytes.read - last.msg.bytes.index > total.bytes %/% 100) {
			lapply(.self$getBiodb()$getObservers(), function(x) x$progress(type = 'info', msg = 'Reading all HMDB metabolites from XML file.', bytes.read, total.bytes))
			last.msg.bytes.index <- bytes.read
		}

		# Is there a complete entry XML (<metabolite>...</metabolite>) in the loaded chunks?
		if (length(grep('</metabolite>', chunk)) > 0 || (length(xml.chunks) > 0 && length(grep('</metabolite>', paste0(xml.chunks[[length(xml.chunks)]], chunk))) > 0)) {

			# Paste all chunks together
			xml <- paste(c(xml.chunks, chunk), collapse = '')

			# Search entry definitions
			match <- stringr::str_match(xml, stringr::regex('^(.*?)(<metabolite>.*</metabolite>)(.*)$', dotall = TRUE))
			if (is.na(match[1, 1]))
				.self$message('error', 'Cannot find matching <metabolite> tag in HMDB XML entries file.')
			metabolites <- match[1, 3]
			xml.chunks <- match[1, 4]

			# Get all metabolites definition
			metabolites <- stringr::str_extract_all(metabolites, stringr::regex('<metabolite>.*?</metabolite>', dotall = TRUE))[[1]]

			# Get IDs
			ids <- stringr::str_match(metabolites, '<accession>(HMDB[0-9]+)</accession>')[, 2]

			# Write all XML entries into files
			.self$getBiodb()$getCache()$saveContentToFile(metabolites, cache.id = .self$getCacheId(), subfolder = 'shortterm', name = ids, ext = .self$getEntryContentType())
		}
		else
			xml.chunks <- c(xml.chunks, chunk)
	}

	# Close file
	close(file.conn)

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

	if (.self$isDownloaded()) {
		# Get IDs from cache
		ids <- .self$getBiodb()$getCache()$listFiles(.self$getCacheId(), subfolder = 'shortterm', ext = .self$getEntryContentType(), extract.name = TRUE)

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

HmdbMetabolitesConn$methods( searchCompound = function(name = NULL, mass = NULL, mass.field = NULL, mass.tol = 0.01, mass.tol.unit = 'plain', max.results = NA_integer_) {
		
	.self$.checkMassField(mass = mass, mass.field = mass.field)

	ids <- NULL

	.self$message('caution', 'HMDB is not searchable. HMDB only provides an HTML interface for searching, giving results split across several pages. It is unpractical to use from a program. Since HMDB is downloaded entirely, a solution using an internal database will be implemented in the future.')

	return(ids)
})

# Private methods {{{1
################################################################

# Get parsing expressions {{{2
################################################################

HmdbMetabolitesConn$methods( .getParsingExpressions = function() {
	return(.BIODB.HMDB.METABOLITES.PARSING.EXPR)
})
