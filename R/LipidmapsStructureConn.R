# vi: fdm=marker

# Constants {{{1
################################################################

.BIODB.LIPIDMAPS.STRUCTURE.PARSING.EXPR = list(
	'name'                  = 'COMMON_NAME',
	'accession'             = 'LM_ID',
	'kegg.compound.id'      = 'KEGG_ID',
	'hmdb.metabolites.id'   = 'HMDBID',
	'chebi.id'              = 'CHEBI_ID',
	'ncbi.pubchem.comp.id'  = 'PUBCHEM_COMPOUND_ID',
	'exact.mass'            = 'MASS',
	'formula'               = 'FORMULA',
	'comp.iupac.name.syst'  = 'SYSTEMATIC_NAME'
)

# Class declaration {{{1
################################################################

#' @include BiodbCompounddbConn.R
#' @include BiodbSearchable.R
#' @include BiodbRemotedbConn.R
LipidmapsStructureConn = methods::setRefClass("LipidmapsStructureConn", contains = c("BiodbRemotedbConn", "BiodbCompounddbConn", "BiodbSearchable"))

# Get entry content request {{{1
################################################################

LipidmapsStructureConn$methods( .doGetEntryContentRequest = function(ids, concatenate = TRUE) {
	return(vapply(ids, function(id) .self$ws.LMSDRecord(lmid = id, mode = 'File', output.type = 'CSV', retfmt = 'request')$getUrl()$toString(), FUN.VALUE = ''))
})

# Get entry page url {{{1
################################################################

LipidmapsStructureConn$methods( getEntryPageUrl = function(id) {
	return(vapply(id, function(x) BiodbUrl(url = .self$getPropValSlot('urls', 'base.url'), params = list(LMID = x))$toString(), FUN.VALUE = ''))
})

# Get entry image url {{{1
################################################################

LipidmapsStructureConn$methods( getEntryImageUrl = function(id) {
	return(rep(NA_character_, length(id)))
})


# Web service LMSDSearch {{{1
################################################################

LipidmapsStructureConn$methods( ws.LMSDSearch = function(mode = NULL, output.mode = NULL, output.type = NULL, output.delimiter = NULL, output.quote = NULL, output.column.header = NULL, lmid = NULL, name = NULL, formula = NULL, search.type = NULL, smiles.string = NULL, exact.mass = NA_real_, exact.mass.offset = NA_real_, core.class = NULL, main.class = NULL, sub.class = NULL, retfmt = c('plain', 'request', 'parsed', 'ids')) {
	":\n\nCalls LMSDSearch web service. See http://www.lipidmaps.org/data/structure/programmaticaccess.html."

	retfmt = match.arg(retfmt)

	# Set parameters for IDs
	if (retfmt == 'ids') {
		output.mode = 'File'
		output.type = 'TSV'
	}

	# Check parameters
	if ( ! is.null(mode) && ! mode %in% c('ProcessStrSearch', 'ProcessTextSearch', 'ProcessTextOntologySearch'))
		.self$message('error', paste0('Unknown value "', output.mode, '" for output.mode parameter.'))
	if ( ! is.null(output.mode) && ! output.mode %in% c('File'))
		.self$message('error', paste0('Unknown value "', output.mode, '" for output.mode parameter.'))
	if ( ! is.null(output.type) && ! output.type %in% c('TSV', 'CSV', 'SDF'))
		.self$message('error', paste0('Unknown value "', output.type, '" for output.type parameter.'))
	if ( ! is.null(output.delimiter) && ! output.delimiter %in% c('Tab', 'Comma', 'Semicolon'))
		.self$message('error', paste0('Unknown value "', output.delimiter, '" for output.delimiter parameter.'))
	if ( ! is.null(output.quote) && ! output.quote %in% c('Yes', 'No'))
		.self$message('error', paste0('Unknown value "', output.quote, '" for output.quote parameter.'))
	if ( ! is.null(output.column.header) && ! output.column.header %in% c('Yes', 'No'))
		.self$message('error', paste0('Unknown value "', output.column.header, '" for output.column.header parameter.'))

	# Build request
	params = list(Mode = mode)
	if ( ! is.null(output.mode))
		params = c(params, OutputMode = output.mode)
	if ( ! is.null(output.type))
		params = c(params, OutputType = output.type)
	if ( ! is.null(output.delimiter))
		params = c(params, OutputDelimiter = output.delimiter)
	if ( ! is.null(output.quote))
		params = c(params, OutputQuote = output.quote)
	if ( ! is.null(output.column.header))
		params = c(params, OutputColumnHeader = output.column.header)
	if ( ! is.null(lmid))
		params = c(params, LMID = lmid)
	if ( ! is.null(name))
		params = c(params, Name = name)
	if ( ! is.null(formula))
		params = c(params, Formula = formula)
	if ( ! is.null(search.type))
		params = c(params, SearchType = search.type)
	if ( ! is.null(smiles.string))
		params = c(params, SMILESString = smiles.string)
	if ( ! is.null(exact.mass))
		params = c(params, ExactMass = exact.mass)
	if ( ! is.null(exact.mass.offset))
		params = c(params, ExactMassOffSet = exact.mass.offset)
	if ( ! is.null(core.class))
		params = c(params, CoreClass = core.class)
	if ( ! is.null(main.class))
		params = c(params, MainClass = main.class)
	if ( ! is.null(sub.class))
		params = c(params, SubClass = sub.class)
	request = BiodbRequest(method = 'get', url = BiodbUrl(url = c(.self$getPropValSlot('urls', 'base.url'), 'structure', 'LMSDSearch.php'), params = params))
	if (retfmt == 'request')
		return(request)

	# Send request
	results = .self$getBiodb()$getRequestScheduler()$sendRequest(request)

	# Parse
	if (retfmt != 'plain' && output.mode == 'File') {

		# Mode must be set or HTML will be output
		if (is.null(output.type) || output.type %in% c('TSV', 'CSV')) {
			if (is.null(output.type) || output.type == 'TSV')
				sep = "\t"
			else
				sep = if (is.null(output.delimiter) || output.delimiter == 'Comma') ',' else ';'
			header = (is.null(output.column.header) || output.column.header == 'Yes')
			quote = if (is.null(output.quote) || output.quote == 'No') '' else '"'
			results = read.table(text = results, sep = sep, header = header, comment.char = '', stringsAsFactors = FALSE, quote = quote, fill = TRUE)
		}
		else
			.self$message('error', 'Only TSV and CSV output types are parsable.')

		# Extract IDs
		if (retfmt == 'ids')
			results = results[['LM_ID']]
	}

	return(results)
})

# Web service LMSDRecord {{{1
################################################################

LipidmapsStructureConn$methods( ws.LMSDRecord = function(lmid, mode = NULL, output.type = NULL, output.delimiter = NULL, output.quote = NULL, output.column.header = NULL, retfmt = c('plain', 'request', 'parsed')) {
	":\n\nCalls LMSDRecord web service. See http://www.lipidmaps.org/data/structure/programmaticaccess.html."

	retfmt = match.arg(retfmt)

	# Check parameters
	if ( ! is.null(mode) && ! mode %in% c('File', 'Download'))
		.self$message('error', paste0('Unknown value "', mode, '" for mode parameter.'))
	if ( ! is.null(output.type) && ! output.type %in% c('TSV', 'CSV', 'SDF', 'MDLMOL'))
		.self$message('error', paste0('Unknown value "', output.type, '" for output.type parameter.'))
	if ( ! is.null(output.delimiter) && ! output.delimiter %in% c('Tab', 'Comma', 'Semicolon'))
		.self$message('error', paste0('Unknown value "', output.delimiter, '" for output.delimiter parameter.'))
	if ( ! is.null(output.quote) && ! output.quote %in% c('Yes', 'No'))
		.self$message('error', paste0('Unknown value "', output.quote, '" for output.quote parameter.'))
	if ( ! is.null(output.column.header) && ! output.column.header %in% c('Yes', 'No'))
		.self$message('error', paste0('Unknown value "', output.column.header, '" for output.column.header parameter.'))

	# Build request
	url = paste0(.self$getPropValSlot('urls', 'base.url'), 'LMSDRecord.php')
	params = list(LMID = lmid)
	if ( ! is.null(mode))
		params = c(params, Mode = mode)
	if ( ! is.null(output.type))
		params = c(params, OutputType = output.type)
	if ( ! is.null(output.delimiter))
		params = c(params, OutputDelimiter = output.delimiter)
	if ( ! is.null(output.quote))
		params = c(params, OutputQuote = output.quote)
	if ( ! is.null(output.column.header))
		params = c(params, OutputColumnHeader = output.column.header)
	request = BiodbRequest(method = 'get', url = BiodbUrl(url = url, params = params))
	if (retfmt == 'request')
		return(request)

	# Send request
	results = .self$getBiodb()$getRequestScheduler()$sendRequest(request)

	# Parse
	if (retfmt != 'plain' && mode %in% c('File', 'Download')) {

		# Mode must be set or HTML will be output
		if ( ! is.null(output.type) && output.type %in% c('TSV', 'CSV')) {
			if (output.type == 'TSV')
				sep = "\t"
			else
				sep = if (is.null(output.delimiter) || output.delimiter == 'Comma') ',' else ';'
			header = (is.null(output.column.header) || output.column.header == 'Yes')
			quote = if (is.null(output.quote) || output.quote == 'No') '' else '"'
			results = read.table(text = results, sep = sep, header = header, comment.char = '', stringsAsFactors = FALSE, quote = quote, fill = TRUE)
		}
		else
			.self$message('error', 'Only TSV and CSV output types are parsable.')
	}

	return(results)
})


# Search by name {{{1
################################################################

LipidmapsStructureConn$methods( searchByName = function(name, max.results = NA_integer_) {
	return(.self$searchCompound(name = name, max.results = max.results))
})

# Search compound {{{1
################################################################

LipidmapsStructureConn$methods( searchCompound = function(name = NULL, mass = NULL, mass.field = NULL, mass.tol = 0.01, mass.tol.unit = 'plain', max.results = NA_integer_) {
		
	.self$.checkMassField(mass = mass, mass.field = mass.field)

	exact.mass = NULL
	exact.mass.offset = NULL

	# Mass search
	if ( ! is.null(mass) && ! is.null(mass.field)) {

		mass.field = .self$getBiodb()$getEntryFields()$getRealName(mass.field)

		if (mass.field != 'monoisotopic.mass')
			.self$message('caution', paste0('Mass field "', mass.field, '" is not handled.'))
		else {
			exact.mass = mass
			if (mass.tol.unit == 'ppm')
				exact.mass.offset = mass * mass.tol * 1e-6
			else
				exact.mass.offset = mass.tol
		}
	}

	# Search
	ids = .self$ws.LMSDSearch(mode = 'ProcessStrSearch', output.mode = 'File', name = name, exact.mass = exact.mass, exact.mass.offset = exact.mass.offset, retfmt = 'ids')

	return(ids)
})

# Private methods {{{1
################################################################

# Get parsing expressions {{{2
################################################################

LipidmapsStructureConn$methods( .getParsingExpressions = function() {
	return(.BIODB.LIPIDMAPS.STRUCTURE.PARSING.EXPR)
})

# Get entry ids {{{2
################################################################

LipidmapsStructureConn$methods( .doGetEntryIds = function(max.results = NA_integer_) {

	# Retrieve all IDs
	ids = .self$ws.LMSDSearch(mode = 'ProcessStrSearch', output.mode = 'File', retfmt = 'ids')

	return(ids)
})

