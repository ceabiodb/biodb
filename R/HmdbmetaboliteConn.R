# vi: fdm=marker

# Class declaration {{{1
################################################################

HmdbmetaboliteConn <- methods::setRefClass("HmdbmetaboliteConn", contains = "RemotedbConn")

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

# Get entry ids {{{1
################################################################

HmdbmetaboliteConn$methods( getEntryIds = function(max.results = NA_integer_) {

	ids <- NULL

	# TODO Needs first to create BiodbConfig and BiodbCache classes.
	# TODO Rename HmdbmetaboliteConn to HmdbmetaboliteConn

	# Do we allow database download? This can take some time.
	if (.self$getBiodb()$getConfig()$enabled(CFG.DBDWNLD)) {

		# Download entries if not already done. --> downloadzip http://www.hmdb.ca/system/downloads/current/hmdb_metabolites.zip
		# Expand zip, and copy files into cache folder (remove old files first).
		# TODO How to know it has not already been downloaded?
		# List files to get all entry IDs
		# Count files to get number of entries
	}

	return(ids)
})
