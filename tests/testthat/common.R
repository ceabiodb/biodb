# vi: fdm=marker

# Constants {{{1
################################################################

ENV <- Sys.getenv()

TEST.DIR <- file.path(getwd(), '..')
OUTPUT.DIR <- file.path(TEST.DIR, 'output')
RES.DIR  <- file.path(TEST.DIR, 'res')
REF.FILES.DIR <- file.path(RES.DIR, 'ref-files')
USERAGENT <- 'biodb.test ; pierrick.roger@cea.fr'

MASSFILEDB.URL <- file.path(RES.DIR, 'mass.csv.file.tsv')
MASSFILEDB.WRONG.HEADER.URL <- file.path(RES.DIR, 'mass.csv.file-wrong_header.tsv')
MASSFILEDB.WRONG.NB.COLS.URL <- file.path(RES.DIR, 'mass.csv.file-wrong_nb_cols.tsv')

MASS.SQLITE.URL = file.path(OUTPUT.DIR, 'mass.sqlite.file.sqlite')

# Create output directory {{{1
################################################################

if ( ! file.exists(OUTPUT.DIR))
	dir.create(OUTPUT.DIR)

# Set databases to test {{{1
################################################################

DATABASES.ALL <- 'all'
DATABASES.NONE <- 'none'

# Instantiate biodb
tmpbiodb <- biodb::Biodb()
dbinf <- tmpbiodb$getDbsInfo()

# List databases to test
TEST.DATABASES <- dbinf$getIds()
if ('DATABASES' %in% names(ENV) && nchar(ENV[['DATABASES']]) > 0) {
	if (tolower(ENV[['DATABASES']]) == DATABASES.NONE)
		TEST.DATABASES <- character(0)
	else if (tolower(ENV[['DATABASES']]) == DATABASES.ALL)
		TEST.DATABASES <- dbinf$getIds()
	else {
		TEST.DATABASES <- strsplit(ENV[['DATABASES']], ',')[[1]]
        
        # Check that databases exist
		db.exists <- vapply(TEST.DATABASES, function(x) dbinf$isDefined(x), FUN.VALUE = TRUE)
		if ( ! all(db.exists)) {
			wrong.dbs <- TEST.DATABASES[ ! db.exists]
			stop(paste0('Cannot run tests, the following database(s) is/are unknown: ', paste(wrong.dbs, collapse = ', ')), '.')
		}
        
        # Check that databases are enabled
		db.disabled <- vapply(TEST.DATABASES, function(x) dbinf$get(x)$getPropertyValue('disabled'), FUN.VALUE = TRUE)
		if (any(db.disabled)) {
			wrong.dbs <- TEST.DATABASES[db.disabled]
			stop(paste0('Cannot run tests, the following database(s) is/are disabled: ', paste(wrong.dbs, collapse = ', ')), '.')
		}
	}
}

# Do not test disabled databases
fct <- function(x) dbinf$get(x)$getPropertyValue('disabled')
to_remove <- vapply(TEST.DATABASES, fct, FUN.VALUE=TRUE)
TEST.DATABASES <- TEST.DATABASES[ ! to_remove]

# Terminate biodb instance
tmpbiodb$terminate()
tmpbiodb <- NULL
dbinf <- NULL

# Set test functions {{{1
################################################################

FUNCTION.ALL <- 'all'
if ('FUNCTIONS' %in% names(ENV)) {
	TEST.FUNCTIONS <- strsplit(ENV[['FUNCTIONS']], ',')[[1]]
} else {
	TEST.FUNCTIONS <- FUNCTION.ALL
}

# MsgAcknowledger observer class {{{1
################################################################

# This observer is used to call a testthat::expect_*() method each time a
# message is received. This is used when running tests on Travis-CI, so Travis
# does not stop tests because no change is detected in output.

MsgAcknowledger <- methods::setRefClass('MsgAcknowledger',
    contains = 'BiodbObserver',
    fields = list(
        .last.index = 'numeric'
        ),

    methods=list(

# Initialize {{{2
################################################################

initialize=function(...) {

	callSuper(...)

	.last.index <<- 0
},

# Msg {{{2
################################################################

msg=function(type='info', msg, class=NA_character_, method=NA_character_,
               lvl=1) {
	testthat::expect_is(msg, 'character')
},

# Progress {{{2
################################################################

progress=function(type='info', msg, index, first, total=NA_character_,
                    lvl=1) {

	.self$checkMessageType(type)

	if (first)
		.last.index <<- 0

	testthat::expect_true(index > .self$.last.index,
                        paste0("Index ", index, " is not greater than last ",
                               "index ", .self$.last.index, ' for progress ',
                               'message "', msg, '", with total ', total, '.'))
	if ( ! is.na(total))
		testthat::expect_true(index <= total,
                             paste0("Index ", index, ' is greater than total ',
                                    total, ' for progress message "', msg,
                                    '".'))

	.last.index <<- index
}

))

# Get log file descriptor {{{1
################################################################

get.log.file.descriptor <- function() {

	if ( ! exists('LOG.FD'))
		assign("LOG.FD", file(file.path(TEST.DIR, 'test.log'), open = 'w'), pos = .GlobalEnv)

	return(LOG.FD)
}

# Create Biodb instance {{{1
################################################################

create.biodb.instance <- function() {

	# Create logger
	logger <- BiodbLogger(file = get.log.file.descriptor(), close.file = FALSE)
	logger$setLevel('caution', 2L)
	logger$setLevel('debug', 2L)
	logger$setLevel('info', 2L)
	logger$setLevel('error', 2L)
	logger$setLevel('warning', 2L)

	# Create test observer
	test.observer <- MsgAcknowledger()

	# Create instance
	biodb <- Biodb$new()
	biodb$addObservers(test.observer)
	biodb$addObservers(logger)

	# Set user agent
	biodb$getConfig()$set('useragent', USERAGENT)

	# Online
	biodb$getConfig()$disable('cache.read.only')
	biodb$getConfig()$enable('allow.huge.downloads')
#	biodb$getConfig()$disable('offline')
	biodb$getConfig()$enable('cache.subfolders')

	return(biodb)
}

# Set test context {{{1
################################################################

set.test.context <- function(biodb, text) {

	# Set testthat context
	context(text)

	# Print banner in log file
	biodb$message('info', "")
	biodb$message('info', "****************************************************************")
	biodb$message('info', paste("Test context", text, sep = " - "))
	biodb$message('info', "****************************************************************")
	biodb$message('info', "")
}


# List reference entries {{{1
################################################################

list.ref.entries <- function(db) {

	# List json files
	files <- Sys.glob(file.path(RES.DIR, paste('entry', db, '*.json', sep = '-')))
	if (length(files) == 0)
		stop(paste0("No JSON reference files for database ", db, "."))

	# Extract ids
	ids <- sub(paste('^.*/entry', db, '(.+)\\.json$', sep = '-'), '\\1', files, perl = TRUE)

	# Replace encoded special characters
	ids = gsub('%3a', ':', ids)

	return(ids)
}

# Load ref entry {{{1
################################################################

load.ref.entry <- function(db, id) {

	# Replace forbidden characters
	id = gsub(':', '%3a', id)

	# Entry file
	file <- file.path(RES.DIR, paste('entry-', db, '-', id, '.json', sep = ''))
	expect_true(file.exists(file), info = paste0('Cannot find file "', file, '" for ', db, ' reference entry', id, '.'))

	# Load JSON
	json <- jsonlite::fromJSON(file)

	# Set NA values
	for (n in names(json))
		if (length(json[[n]]) == 1) {
			if (json[[n]] == 'NA_character_')
				json[[n]] <- NA_character_
		}

	return(json)
}

# Load reference entries {{{1
################################################################

load.ref.entries <- function(db) {

	entries.desc <- NULL

	# List JSON files
	entry.json.files <- Sys.glob(file.path(RES.DIR, paste('entry', db, '*.json', sep = '-')))

	# Loop on all JSON files
	for (f in entry.json.files) {

		# Load entry from JSON
		entry <- jsonlite::read_json(f)

		# Replace NULL values by NA
		entry <- lapply(entry, function(x) if (is.null(x)) NA else x)

		# Convert to data frame
		entry.df <- as.data.frame(entry, stringsAsFactors = FALSE)

		# Append entry to main data frame
		entries.desc <- plyr::rbind.fill(entries.desc, entry.df)
	}

	return(entries.desc)
}


# Create connector for generic tests {{{1
################################################################

create.conn.for.generic.tests = function(biodb, class.db) {

	# Get connector
	if (biodb$getFactory()$connExists(class.db))
		conn = biodb$getFactory()$getConn(class.db)

	# Create connector
	else {
		conn = biodb$getFactory()$createConn(class.db)

		# Set parameters for local connectors
		if (class.db == 'mass.csv.file') {
			conn$setUrl('base.url', MASSFILEDB.URL)
			conn$setField('accession', c('compound.id', 'ms.mode', 'chrom.col.name', 'chrom.rt'))
			biodb$getCache()$deleteFiles(cache.id = conn$getCacheId(), subfolder = 'shortterm') # Make sure we have no residual cache entries from previous tests
		}
		else if (class.db == 'mass.sqlite') {
			conn$setUrl('base.url', MASS.SQLITE.URL)
			biodb$getCache()$deleteFiles(cache.id = conn$getCacheId(), subfolder = 'shortterm') # Make sure we have no residual cache entries from previous tests

			# Create SQLite database file
			if ( ! file.exists(MASS.SQLITE.URL)) {

				mass.csv.file.conn = create.conn.for.generic.tests(biodb = biodb, class.db = 'mass.csv.file')
				conn$allowEditing()
				biodb$copyDb(conn.from = mass.csv.file.conn, conn.to = conn)
				conn$allowWriting()
				conn$write()
				conn$disallowWriting()
				conn$disallowEditing()
			}
		}

		# Create needed additional connectors for computing missing fields
		for (field in biodb$getEntryFields()$getFieldNames())
			for (computing.db in biodb$getEntryFields()$get(field)$getComputableFrom())
				if (computing.db != class.db)
					c = create.conn.for.generic.tests(biodb = biodb, class.db = computing.db)
	}

	return(conn)
}

# Run test_that method {{{1
################################################################

test.that  <- function(msg, fct, biodb = NULL, obs = NULL, conn = NULL) {

	if (TEST.FUNCTIONS == FUNCTION.ALL || fct %in% TEST.FUNCTIONS) {

		# Send message to logger
		biodb.instance <- if (is.null(conn)) biodb else conn$getBiodb()
		if (is.null(biodb.instance))
			stop("You must at least set the biodb parameter in order to send message to logger.")
		biodb.instance$message('info', '')
		biodb.instance$message('info', paste('Running test function ', fct, ' ("', msg, '").'))
		biodb.instance$message('info', '----------------------------------------------------------------')
		biodb.instance$message('info', '')

		# Call test function
		if ( ! is.null(biodb) && ! is.null(obs))
			test_that(msg, do.call(fct, list(biodb = biodb, obs = obs)))
		else if ( ! is.null(biodb))
			test_that(msg, do.call(fct, list(biodb)))
		else if ( ! is.null(conn) && ! is.null(obs))
			test_that(msg, do.call(fct, list(conn = conn, obs = obs)))
		else if ( ! is.null(conn))
			test_that(msg, do.call(fct, list(conn)))
		else
			stop(paste0('Do not know how to call test function "', fct, '".'))
	}
}

# MsgRecorder observer class {{{1
################################################################################

MsgRecorder <- methods::setRefClass("MsgRecorder",
    contains = "BiodbObserver",
    fields = list(
                  .msgs='character',
                  .msgs.by.type='list'
                  ),
    methods=list(

# Initialize {{{2
################################################################################

initialize=function(...) {
	.msgs <<- character()
	.msgs.by.type <<- list()
},

# Msg {{{2
################################################################################

msg=function(type='info', msg, class=NA_character_, method=NA_character_,
             lvl=1) {
	.msgs <<- c(.self$.msgs, msg)
	.self$.msgs.by.type[[type]] <- c(.self$.msgs.by.type[[type]], msg)
},

# hasMsgs {{{2
################################################################################

hasMsgs=function(type = NULL) {

	f = FALSE

	if (is.null(type))
		f = (length(.self$.msg) > 0)
	else
		f = if (type %in% names(.self$.msgs.by.type)) (length(.self$.msgs.by.type[[type]])) else FALSE

	return(f)
},

# lastMsg {{{2
################################################################################

lastMsg = function() {

    m <- NA_character_

    i <- length(.self$.msgs)
    if (i > 0)
        m <- .self$.msgs[[i]]

	return(m)
},

# getLastMsgByType {{{2
################################################################################

getLastMsgByType = function(type) {
	m = NULL
	if (type %in% names(.self$.msgs.by.type)) {
		m = .self$.msgs.by.type[[type]]
		m = m[[length(m)]]
	}
	return(m)
},

# getMsgsByType {{{2
################################################################################

getMsgsByType = function(type) {
	msgs = character()

	if ( ! is.null(type) && type %in% names(.self$.msgs.by.type))
		msgs = .self$.msgs.by.type[[type]]

	return(msgs)
},

# clearMessages {{{2
################################################################################

clearMessages = function() {
	.msgs <<- character()
	.msgs.by.type <<- list()
}

))

# Add message recorder observer {{{1
################################################################################

add_msg_recorder_obs <- function(biodb) {

    # Create observer
    obs <- MsgRecorder()

	# Set observer
	biodb$addObservers(obs)

	return(obs)
}
