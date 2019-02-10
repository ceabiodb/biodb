# vi: fdm=marker

# Constants {{{1
################################################################

.BIODB.UNIPROT.PARSING.EXPR <- list(
	'name'              = "/uniprot:uniprot/uniprot:entry/uniprot:name",
	'gene.symbols'      = "//uniprot:gene/uniprot:name",
	'sequence'          = "//uniprot:entry/uniprot:sequence",
	'accession'         = "//uniprot:accession[1]",
	'kegg.compound.id'  = list(path = "//uniprot:dbReference[@type='KEGG']", attr = 'id'),
	'ncbi.gene.id'      = list(path = "//uniprot:dbReference[@type='GeneID']", attr = 'id'),
	'expasy.enzyme.id'  = list(path = "//uniprot:dbReference[@type='EC']", attr = 'id'),
	'molecular.mass'    = list(path = "//uniprot:entry/uniprot:sequence", attr = 'mass'),
	'length'            = list(path = "//uniprot:entry/uniprot:sequence", attr = 'length')
)

# Class declaration {{{1
################################################################

#' The connector class to Uniprot database.
#'
#' This is a concrete connector class. It must never be instantiated directly, but instead be instantiated through the factory \code{\link{BiodbFactory}}. Only specific methods are described here. See super classes for the description of inherited methods.
#'
#' @param columns   The field columns to retrieve from the database.
#' @param format    The return format.
#' @param query     The query to send to the database.
#'
#' @seealso \code{\link{BiodbFactory}}, \code{\link{RemotedbConn}}, \code{\link{CompounddbConn}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Get Uniprot connector
#' uniprot <- mybiodb$getFactory()$createConn('uniprot')
#'
#' # Access web service query
#' result <- uniprot$ws.query(query = 'name:"prion protein"',
#'                            columns = c('id', 'entry name'),
#'                            format = 'txt', limit = 10)
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @include CompounddbConn.R
#' @include RemotedbConn.R
#' @export UniprotConn
#' @exportClass UniprotConn
UniprotConn <- methods::setRefClass("UniprotConn", contains = c("RemotedbConn", "CompounddbConn"))

# Constructor {{{1
################################################################

UniprotConn$methods( initialize = function(...) {
	callSuper(...)
})

# Web service query {{{1
################################################################

UniprotConn$methods( ws.query = function(query = '', columns = NULL, format = NULL, limit = NULL, retfmt = c('plain', 'parsed', 'ids', 'request')) {
	":\n\nDirect query to the database for searching for compounds. See http://www.uniprot.org/help/api_queries for details."

	retfmt <- match.arg(retfmt)

	# Set parameters for retrieving IDs
	if (retfmt == 'ids') {
		columns <- 'id'
		format <- 'tab'
	}

	# Set columns
	if (is.null(columns) || is.na(columns))
		columns <- c("citation", "clusters", "comments", "domains", "domain", "ec", "id", "entry name", "existence", "families", "features", "genes", "go", "go-id", "interactor", "keywords", "last-modified", "length", "organism", "organism-id", "pathway", "protein names", "reviewed", "sequence", "3d", "version", "virus hosts")

	# Set format
	if (is.null(format) || is.na(format))
		format <- 'tab'

	# Build request
	params <- list(query = query, columns = columns, format = format)
	if ( ! is.null(limit) && ! is.na(limit))
		params[['limit']] <- limit
	url <- BiodbUrl(url = .self$getUrl('base.url'), params = params)
	request <- BiodbRequest(method = 'get', url = url)

	# Return request
	if (retfmt == 'request')
		return(request)

	# Send request
	results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

	# Parse
	if (retfmt != 'plain') {

		# Parse data frame
		readtc <- textConnection(results, "r", local = TRUE)
		df <- read.table(readtc, sep = "\t", header = TRUE, check.names = FALSE)
		close(readtc)
		results <- df

		# Get IDs
		if (retfmt == 'ids')
			results <- as.character(results[[1]])
	}

	return(results)
})

# Get entry ids {{{1
################################################################

UniprotConn$methods( getEntryIds = function(max.results = NA_integer_) {

	ids <- .self$ws.query(limit = max.results, retfmt = 'ids')

	return(ids)
})

# Do get entry content request {{{1
################################################################

UniprotConn$methods( .doGetEntryContentRequest = function(id, concatenate = TRUE) {
	                    
	url <- paste0(.self$getUrl('base.url'), id, '.xml')

	return(url)
})

# Get entry page url {{{1
################################################################

UniprotConn$methods( getEntryPageUrl = function(id) {
	return(file.path(.self$getUrl('base.url'), id, fsep = '/'))
})

# Get entry image url {{{1
################################################################

UniprotConn$methods( getEntryImageUrl = function(id) {
	return(rep(NA_character_, length(id)))
})

# Search compound {{{1
################################################################

UniprotConn$methods( searchCompound = function(name = NULL, mass = NULL, mass.field = NULL, mass.tol = 0.01, mass.tol.unit = 'plain', max.results = NA_integer_) {
		
	.self$.checkMassField(mass = mass, mass.field = mass.field)

	query <- ''

	# Search for name
	if ( ! is.null(name) && ! is.na(name)) {
		name.query <- paste('name', paste0('"', name, '"'), sep = ':')
		mnemonic.query <- paste('mnemonic', paste0('"', name, '"'), sep = ':')
		query <- paste(name.query, mnemonic.query, sep = ' OR ')
	}

	# Search for mass
	if ( ! is.null(mass) && ! is.null(mass.field)) {

		mass.field <- .self$getBiodb()$getEntryFields()$getRealName(mass.field)

		if (mass.field != 'molecular.mass')
			.self$message('caution', paste0('Mass field "', mass.field, '" is not handled.'))

		else {

			if (mass.tol.unit == 'ppm') {
				mass.min <- mass * (1 - mass.tol * 1e-6)
				mass.max <- mass * (1 + mass.tol * 1e-6)
			} else {
				mass.min <- mass - mass.tol
				mass.max <- mass + mass.tol
			}

			# Uniprot does not accept mass in floating numbers
			uniprot.mass.min <- as.integer(mass.min)
			uniprot.mass.max <- as.integer(mass.max)
			if (uniprot.mass.min != mass.min || uniprot.mass.max != mass.max)
				.self$message('caution', paste0('Uniprot requires integers for mass range. Range [', mass.min, ', ', mass.max, '] will be converted into [', uniprot.mass.min, ', ', uniprot.mass.max, '].'))

			mass.query <- paste0('mass:[', uniprot.mass.min, ' TO ', uniprot.mass.max, ']')

			if (nchar(query) > 0) {
				query <- paste0('(', query, ')')
				query <- paste(query, mass.query, sep = ' AND ')
			}
			else
				query <- mass.query
		}
	}

	# Send query
	ids <- .self$ws.query(query = query, limit = max.results, retfmt = 'ids')

	return(ids)
})

# Private methods {{{1
################################################################

# Get parsing expressions {{{2
################################################################

UniprotConn$methods( .getParsingExpressions = function() {
	return(.BIODB.UNIPROT.PARSING.EXPR)
})
