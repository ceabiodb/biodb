# vi: fdm=marker

# Class declaration {{{1
################################################################

#' @include CompounddbConn.R
#' @include RemotedbConn.R
LipidmapsStructureConn <- methods::setRefClass("LipidmapsStructureConn", contains = c("RemotedbConn", "CompounddbConn"))

# Get entry content url {{{1
################################################################

LipidmapsStructureConn$methods( .doGetEntryContentUrl = function(ids, concatenate = TRUE) {
	return(vapply(ids, function(id) .self$ws.LMSDRecord(lmid = id, output.type = 'CSV', biodb.url = TRUE), FUN.VALUE = ''))
})

# Get entry page url {{{1
################################################################

LipidmapsStructureConn$methods( getEntryPageUrl = function(id) {
	return(paste(.self$getBaseUrl(), '?LMID=', id, sep = ''))
})

# Get entry image url {{{1
################################################################

LipidmapsStructureConn$methods( getEntryImageUrl = function(id) {
	return(rep(NA_character_, length(id)))
})

# Get entry content {{{1
################################################################

LipidmapsStructureConn$methods( getEntryContent = function(entry.id) {

	# Initialize return values
	content <- rep(NA_character_, length(entry.id))

	# Request
	content <- vapply(entry.id, function(x) .self$.getUrlScheduler()$getUrl(.self$getEntryContentUrl(x)), FUN.VALUE = '')

	return(content)
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

# Web service LMSDSearch {{{1
################################################################

LipidmapsStructureConn$methods( ws.LMSDSearch = function(mode = NA_character_, output.mode = NA_character_, output.type = NA_character_, output.delimiter = NA_character_, output.quote = NA_character_, output.column.header = NA_character_, lmid = NA_character_, name = NA_character_, formula = NA_character_, search.type = NA_character_, smiles.string = NA_character_, exact.mass = NA_real_, exact.mass.offset = NA_real_, core.class = NA_character_, main.class = NA_character_, sub.class = NA_character_, biodb.url = FALSE, biodb.parse = FALSE, biodb.ids = FALSE) {
	":\n\nCalls LMSDSearch web service. See http://www.lipidmaps.org/data/structure/programmaticaccess.html."

	# Check parameters
	if ( ! is.na(mode) && ! mode %in% c('ProcessStrSearch', 'ProcessTextSearch', 'ProcessTextOntologySearch'))
		.self$message('error', paste0('Unknown value "', output.mode, '" for output.mode parameter.'))
	if ( ! is.na(output.mode) && ! output.mode %in% c('File'))
		.self$message('error', paste0('Unknown value "', output.mode, '" for output.mode parameter.'))
	if ( ! is.na(output.type) && ! output.type %in% c('TSV', 'CSV', 'SDF'))
		.self$message('error', paste0('Unknown value "', output.type, '" for output.type parameter.'))
	if ( ! is.na(output.delimiter) && ! output.delimiter %in% c('Tab', 'Comma', 'Semicolon'))
		.self$message('error', paste0('Unknown value "', output.delimiter, '" for output.delimiter parameter.'))
	if ( ! is.na(output.quote) && ! output.quote %in% c('Yes', 'No'))
		.self$message('error', paste0('Unknown value "', output.quote, '" for output.quote parameter.'))
	if ( ! is.na(output.column.header) && ! output.column.header %in% c('Yes', 'No'))
		.self$message('error', paste0('Unknown value "', output.column.header, '" for output.column.header parameter.'))

	# Build request
	url <- paste0(.self$getBaseUrl(), 'structure/LMSDSearch.php')
	params <- c(Mode = mode)
	if ( ! is.na(output.mode))
		params <- c(params, OutputMode = output.mode)
	if ( ! is.na(output.type))
		params <- c(params, OutputType = output.type)
	if ( ! is.na(output.delimiter))
		params <- c(params, OutputDelimiter = output.delimiter)
	if ( ! is.na(output.quote))
		params <- c(params, OutputQuote = output.quote)
	if ( ! is.na(output.column.header))
		params <- c(params, OutputColumnHeader = output.column.header)
	if ( ! is.na(lmid))
		params <- c(params, LMID = lmid)
	if ( ! is.na(name))
		params <- c(params, Name = name)
	if ( ! is.na(formula))
		params <- c(params, Formula = formula)
	if ( ! is.na(search.type))
		params <- c(params, SearchType = search.type)
	if ( ! is.na(smiles.string))
		params <- c(params, SMILESString = smiles.string)
	if ( ! is.na(exact.mass))
		params <- c(params, ExactMass = exact.mass)
	if ( ! is.na(exact.mass.offset))
		params <- c(params, ExactMassOffSet = exact.mass.offset)
	if ( ! is.na(core.class))
		params <- c(params, CoreClass = core.class)
	if ( ! is.na(main.class))
		params <- c(params, MainClass = main.class)
	if ( ! is.na(sub.class))
		params <- c(params, SubClass = sub.class)

	# Returns URL
	if (biodb.url)
		return(.self$.getUrlScheduler()$getUrlString(url, params))

	# Send request
	results <- .self$.getUrlScheduler()$getUrl(url, params)

	# Parse
	if ((biodb.parse || biodb.ids) && output.mode == 'File') {
		# Mode must be set or HTML will be output
		if (is.na(output.type) || output.type %in% c('TSV', 'CSV')) {
			if (is.na(output.type) || output.type == 'TSV')
				sep <- "\t"
			else
				sep <- if (is.na(output.delimiter) || output.delimiter == 'Comma') ',' else ';'
			header <- (is.na(output.column.header) || output.column.header == 'Yes')
			quote <- if (is.na(output.quote) || output.quote == 'No') '' else '"'
			results <- read.table(text = results, sep = sep, header = header, comment.char = '', stringsAsFactors = FALSE, quote = quote)
		}
	}

	# Extract IDs
	if (biodb.ids && is.data.frame(results))
		results <- results[['LM_ID']]

	return(results)
})

# Web service LMSDRecord {{{1
################################################################

LipidmapsStructureConn$methods( ws.LMSDRecord = function(lmid, mode = NA_character_, output.type = NA_character_, output.delimiter = NA_character_, output.quote = NA_character_, output.column.header = NA_character_, biodb.url = FALSE, biodb.parse = FALSE) {
	":\n\nCalls LMSDRecord web service. See http://www.lipidmaps.org/data/structure/programmaticaccess.html."

	# Check parameters
	if ( ! is.na(mode) && ! mode %in% c('File', 'Download'))
		.self$message('error', paste0('Unknown value "', mode, '" for mode parameter.'))
	if ( ! is.na(output.type) && ! output.type %in% c('TSV', 'CSV', 'SDF', 'MDLMOL'))
		.self$message('error', paste0('Unknown value "', output.type, '" for output.type parameter.'))
	if ( ! is.na(output.delimiter) && ! output.delimiter %in% c('Tab', 'Comma', 'Semicolon'))
		.self$message('error', paste0('Unknown value "', output.delimiter, '" for output.delimiter parameter.'))
	if ( ! is.na(output.quote) && ! output.quote %in% c('Yes', 'No'))
		.self$message('error', paste0('Unknown value "', output.quote, '" for output.quote parameter.'))
	if ( ! is.na(output.column.header) && ! output.column.header %in% c('Yes', 'No'))
		.self$message('error', paste0('Unknown value "', output.column.header, '" for output.column.header parameter.'))

	# Build request
	url <- paste0(.self$getBaseUrl(), 'LMSDRecord.php')
	params <- c(LMID = lmid)
	if ( ! is.na(mode))
		params <- c(params, Mode = mode)
	if ( ! is.na(output.type))
		params <- c(params, OutputType = output.type)
	if ( ! is.na(output.delimiter))
		params <- c(params, OutputDelimiter = output.delimiter)
	if ( ! is.na(output.quote))
		params <- c(params, OutputQuote = output.quote)
	if ( ! is.na(output.column.header))
		params <- c(params, OutputColumnHeader = output.column.header)

	# Returns URL
	if (biodb.url)
		return(.self$.getUrlScheduler()$getUrlString(url, params))

	# Send request
	results <- .self$.getUrlScheduler()$getUrl(url, params)

	# Parse
	if (biodb.parse && mode %in% c('File', 'Download')) {
		# Mode must be set or HTML will be output
		if (output.type %in% c('TSV', 'CSV')) {
			if (output.type == 'TSV')
				sep <- "\t"
			else
				sep <- if (is.na(output.delimiter) || output.delimiter == 'Comma') ',' else ';'
			header <- (is.na(output.column.header) || output.column.header == 'Yes')
			quote <- if (is.na(output.quote) || output.quote == 'No') '' else '"'
			results <- read.table(text = results, sep = sep, header = header, comment.char = '', stringsAsFactors = FALSE, quote = quote)
		}
	}

	return(results)
})

# PRIVATE METHODS {{{1
################################################################

LipidmapsStructureConn$methods( .parse.returned.csv = function(results, output.type = NA_character_, output.delimiter = NA_character_, output.quote = NA_character_, output.column.header = NA_character_) {
print('-------------------------------- LipidmapsStructureConn::.parse.returned.csv 01')


	return(results)
})
