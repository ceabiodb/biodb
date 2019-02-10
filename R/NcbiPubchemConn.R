# vi: fdm=marker

# Class declaration {{{1
################################################################

#' @include NcbiEntrezConn.R
NcbiPubchemConn <- methods::setRefClass("NcbiPubchemConn", contains = 'NcbiEntrezConn', fields = list(.db.name = 'character', .id.xmltag = 'character', .entry.xmltag = 'character', .id.urlfield = 'character'))

# Constructor {{{1
################################################################

NcbiPubchemConn$methods( initialize = function(db.name, id.xmltag, entry.xmltag, id.urlfield, ...) {

	# Call parent constructor
	callSuper(...)
	.self$.abstract.class('NcbiPubchemConn')

	.db.name <<- db.name
	.id.xmltag <<- id.xmltag
	.entry.xmltag <<- entry.xmltag
	.id.urlfield <<- id.urlfield
})

# Do get entry content request {{{1
################################################################

NcbiPubchemConn$methods( .doGetEntryContentRequest = function(id, concatenate = TRUE) {

	if (concatenate)
		url <- paste0(file.path(.self$getUrl('ws.url'), .self$.db.name, fsep = '/'), '/', .self$.id.urlfield, '/', paste(id, collapse = ','), '/XML')
	else
		url <- paste0(file.path(.self$getUrl('ws.url'), .self$.db.name, fsep = '/'),'/', .self$.id.urlfield, '/', id, '/XML')

	return(url)
})

# Get entry page url {{{1
################################################################

NcbiPubchemConn$methods( getEntryPageUrl = function(id) {
	return(file.path(.self$getUrl('base.url'), .self$.db.name, id, fsep = '/'))
})

# Get entry image url {{{1
################################################################

NcbiPubchemConn$methods( getEntryImageUrl = function(id) {

	urls <- rep(NA_character_, length(id))

	# Loop on all IDs
	i = 0
	for(x in id) {

		i = i + 1

		# Set params
		params = list()
		params[[.self$.id.urlfield]] = x
		params$t = 'l'

		# Build URL
		urls[[i]] = BiodbUrl(url = file.path(.self$getUrl('base.url'), 'image', 'imgsrv.fcgi', fsep = '/'), params = params)$toString()
	}

	return(urls)
})

# Get entry content {{{1
################################################################

NcbiPubchemConn$methods( getEntryContent = function(entry.id) {

	# Debug
	.self$message('info', paste0("Get entry content(s) for ", length(entry.id)," id(s)..."))

	URL.MAX.LENGTH <- 2048
	concatenate <- TRUE
	done <- FALSE

	while ( ! done) {

		done <- TRUE

		# Initialize return values
		content <- rep(NA_character_, length(entry.id))

		# Get URL requests
		url.requests <- .self$getEntryContentRequest(entry.id, concatenate = concatenate, max.length = URL.MAX.LENGTH)

		# Loop on all URLs
		for (url in url.requests) {

			# Send request
			xmlstr <- .self$getBiodb()$getRequestScheduler()$getUrl(url)

			if (is.na(xmlstr) || length(grep('PUGREST.BadRequest|PUGREST.NotFound', xmlstr)) > 0) {
				if (concatenate) {
					.self$message('caution', "One of the IDs to retrieve is wrong.")
					concatenate <- FALSE
					done <- FALSE
					break
				}
				next
			}

			# Parse XML
			xml <-  XML::xmlInternalTreeParse(xmlstr, asText = TRUE)

			# Get returned IDs
			ns <- c(pcns = "http://www.ncbi.nlm.nih.gov")
			returned.ids <- XML::xpathSApply(xml, paste0("//pcns:", .self$.id.xmltag), XML::xmlValue, namespaces = ns)

			# Store contents
			content[match(returned.ids, entry.id)] <- vapply(XML::getNodeSet(xml, paste0("//pcns:", .self$.entry.xmltag), namespaces = ns), XML::saveXML, FUN.VALUE = '')
		}
	}

	return(content)
})
