# vi: fdm=marker

# Class declaration {{{1
################################################################

LipidmapsStructureConn <- methods::setRefClass("LipidmapsstructureConn", contains = "RemotedbConn")

# Constructor {{{1
################################################################

LipidmapsStructureConn$methods( initialize = function(...) {
	# From http://www.lipidmaps.org/data/structure/programmaticaccess.html:
	# If you write a script to automate calls to LMSD, please be kind and do not hit our server more often than once per 20 seconds. We may have to kill scripts that hit our server more frequently.
	callSuper(content.type = BIODB.CSV, base.url = 'http://www.lipidmaps.org/data/', scheduler = UrlRequestScheduler$new(t = 20, parent = .self), ...)
})

# Get entry content url {{{1
################################################################

LipidmapsStructureConn$methods( .doGetEntryContentUrl = function(id, concatenate = TRUE) {
	return(paste(.self$getBaseUrl(), 'LMSDRecord.php', '?Mode=File&LMID=', id, '&OutputType=CSV&OutputQuote=No', sep = ''))
})

# Get entry page url {{{1
################################################################

LipidmapsStructureConn$methods( getEntryPageUrl = function(id) {
	return(paste(.self$getBaseUrl(), '?LMID=', id, sep = ''))
})

# Get entry content {{{1
################################################################

LipidmapsStructureConn$methods( getEntryContent = function(id) {

	# Initialize return values
	content <- rep(NA_character_, length(id))

	# Request
	content <- vapply(id, function(x) .self$.getUrlScheduler()$getUrl(.self$getEntryContentUrl(x)), FUN.VALUE = '')

	return(content)
})

# Create entry {{{1
################################################################

LipidmapsStructureConn$methods( createEntry = function(content, drop = TRUE) {

	entries <- list()

	# Mapping column names
	col2field <- list()
	col2field[[BIODB.NAME]] <- 'COMMON_NAME'
	col2field[[BIODB.ACCESSION]] <- 'LM_ID'
	col2field[[BIODB.KEGG.COMPOUND.ID]] <- 'KEGG_ID'
	col2field[[BIODB.HMDB.METABOLITE.ID]] <- 'HMDBID'
	col2field[[BIODB.MASS]] <- 'MASS'
	col2field[[BIODB.FORMULA]] <- 'FORMULA'
	
	for (text in content) {

		# Create instance
		entry <- BiodbEntry$new(biodb = .self$getBiodb())

		# Split text in lines
		lines <- strsplit(text, "\n")[[1]]

		# An error occured
		if ( ! grepl("No record found", lines[[2]])) {

			# Keys on first line
			keys <- strsplit(lines[[1]], ',')[[1]]

			# Values on second line
			values <- strsplit(lines[[2]], ',')[[1]]
			names(values) <- keys[seq(values)]

			# Get field values
			for (field in names(col2field))
				if (values[[col2field[[field]]]] != '-')
					entry$setField(field, values[[col2field[[field]]]])

			# Set names
			if (values[['SYNONYMS']] != '-') {
				# TODO
			}
		}

		entries <- c(entries, entry)
	}

	# Replace elements with no accession id by NULL
	entries <- lapply(entries, function(x) if (is.na(x$getField(BIODB.ACCESSION))) NULL else x)

	# If the input was a single element, then output a single object
	if (drop && length(content) == 1)
		entries <- entries[[1]]

	return(entries)
})

# Get entry ids {{{1
################################################################

LipidmapsStructureConn$methods( getEntryIds = function(max.results = NA_integer_) {

	# Retrieve all entries
	result.txt <- .self$.getUrlScheduler()$getUrl(paste(.self$getBaseUrl(), 'structure/LMSDSearch.php?Mode=ProcessStrSearch&OutputMode=File', sep = ''))

	# Convert into data frame
	result.df <- read.table(text = result.txt, sep = "\t", header = TRUE, comment.char = '', stringsAsFactors = FALSE, quote = '')

	# Extract IDs
	ids <- result.df[['LM_ID']]

	# Cut
	if ( ! is.na(max.results) && length(ids) > max.results)
		ids <- ids[1:max.results]

	return(ids)
})
