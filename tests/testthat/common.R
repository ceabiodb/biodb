# vi: fdm=marker

# Constants {{{1
################################################################

ENV <- Sys.getenv()

TEST.DIR <- file.path(getwd(), '..')
OUTPUT.DIR <- file.path(TEST.DIR, 'output')
RES.DIR  <- file.path(TEST.DIR, 'res')
REF.FILES.DIR <- file.path(RES.DIR, 'ref-files')
OFFLINE.CACHE.DIR <- file.path(RES.DIR, 'offline-cache')
ONLINE.CACHE.DIR <- ENV[['BIODB_CACHE_DIRECTORY']]
USERAGENT <- 'biodb.test ; pk.roger@icloud.com'

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

TEST.DATABASES <- biodb::Biodb$new(logger = FALSE)$getDbsInfo()$getIds()
if ('DATABASES' %in% names(ENV) && nchar(ENV[['DATABASES']]) > 0) {
	if (tolower(ENV[['DATABASES']]) == DATABASES.NONE)
		TEST.DATABASES <- character(0)
	else if (tolower(ENV[['DATABASES']]) == DATABASES.ALL)
		TEST.DATABASES <- biodb::Biodb$new(logger = FALSE)$getDbsInfo()$getIds()
	else {
		TEST.DATABASES <- strsplit(ENV[['DATABASES']], ',')[[1]]
		db.exists <- vapply(TEST.DATABASES, function(x) biodb::Biodb$new(logger = FALSE)$getDbsInfo()$isDefined(x), FUN.VALUE = TRUE)
		if ( ! all(db.exists)) {
			wrong.dbs <- TEST.DATABASES[ ! db.exists]
			stop(paste('Unknown database(s) ', paste(wrong.dbs, collapse = ', ')), '.', sep = '')
		}
	}
}

# Remove databases to test
if ('DONT_TEST_DBS' %in% names(ENV) && nchar(ENV[['DONT_TEST_DBS']]) > 0) {
	DONT.TEST.DBS <- strsplit(ENV[['DONT_TEST_DBS']], ',')[[1]]
	db.exists <- vapply(DONT.TEST.DBS, function(x) biodb::Biodb$new(logger = FALSE)$getDbsInfo()$isDefined(x), FUN.VALUE = TRUE)
	if ( ! all(db.exists)) {
		wrong.dbs <- DONT.TEST.DBS[ ! db.exists]
		stop(paste('Unknown database(s) ', paste(wrong.dbs, collapse = ', ')), '.', sep = '')
	}
	TEST.DATABASES <- TEST.DATABASES[ ! TEST.DATABASES %in% DONT.TEST.DBS]
}

# Set testing modes {{{1
################################################################

MODE.OFFLINE <- 'offline'
MODE.ONLINE <- 'online'
MODE.QUICK.ONLINE <- 'quick.online'
MODE.ALL <- 'all'
MODE.FULL <- 'full'
DEFAULT.MODES <- MODE.OFFLINE
ALLOWED.MODES <- c(MODE.ONLINE, MODE.QUICK.ONLINE, MODE.OFFLINE)
if ('MODES' %in% names(ENV) && nchar(ENV[['MODES']]) > 0) {
	if (ENV[['MODES']] %in% c(MODE.ALL, MODE.FULL))
		TEST.MODES <- ALLOWED.MODES
	else {
		TEST.MODES <- strsplit(ENV[['MODES']], ',')[[1]]
		mode.exists <- TEST.MODES %in% ALLOWED.MODES
		if ( ! all(mode.exists)) {
			wrong.modes <- TEST.MODES[ ! mode.exists]
			stop(paste('Unknown testing mode(s) ', paste(wrong.modes, collapse = ', ')), '.', sep = '')
		}
	}
} else {
	TEST.MODES <- DEFAULT.MODES
}

# Set test functions {{{1
################################################################

FUNCTION.ALL <- 'all'
if ('FUNCTIONS' %in% names(ENV)) {
	TEST.FUNCTIONS <- strsplit(ENV[['FUNCTIONS']], ',')[[1]]
} else {
	TEST.FUNCTIONS <- FUNCTION.ALL
}

# Test observer {{{1
################################################################

TestObserver <- methods::setRefClass('TestObserver', contains = 'BiodbObserver', fields = list(.last.index = 'numeric'))

TestObserver$methods( initialize = function(...) {

	callSuper(...)

	.last.index <<- 0
})

TestObserver$methods( progress = function(type = 'info', msg, index, total, first) {

	.self$checkMessageType(type)

	if (first)
		.last.index <<- 0

	testthat::expect_gt(index, .self$.last.index)
	testthat::expect_lte(index, total)

	.last.index <<- index
})

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
	logger$includeMsgType('debug')

	# Create test observer
	test.observer <- TestObserver()

	# Create instance
	biodb <- Biodb$new(logger = FALSE, observers = c(test.observer, logger))

	# Set user agent
	biodb$getConfig()$set('useragent', USERAGENT)

	# Set Peakforest URL and token
	if ('BIODB_TEST_PEAKFOREST_TOKEN' %in% names(ENV))
		for (db in c('peakforest.mass', 'peakforest.compound'))
			biodb$getDbsInfo()$get(db)$setToken(ENV[['BIODB_PEAKFOREST_ALPHA_TOKEN']])
	if ('BIODB_TEST_PEAKFOREST_URL' %in% names(ENV))
		for (db in c('peakforest.mass', 'peakforest.compound'))
			biodb$getDbsInfo()$get(db)$setBaseUrl(ENV[['BIODB_TEST_PEAKFOREST_URL']])

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

# Set mode {{{1
################################################################

set.mode <- function(biodb, mode) {

	# Online
	if (mode == MODE.ONLINE) {
		biodb$getConfig()$set('cache.directory', ONLINE.CACHE.DIR)
		biodb$getConfig()$disable('cache.read.only')
		biodb$getConfig()$enable('allow.huge.downloads')
		biodb$getConfig()$disable('offline')
		biodb$getConfig()$enable('cache.subfolders')
	}

	# Quick online
	else if (mode == MODE.QUICK.ONLINE) {
		biodb$getConfig()$set('cache.directory', ONLINE.CACHE.DIR)
		biodb$getConfig()$disable('cache.read.only')
		biodb$getConfig()$enable('allow.huge.downloads')
		biodb$getConfig()$disable('offline')
		biodb$getConfig()$enable('cache.subfolders')
	}

	# Offline
	else if (mode == MODE.OFFLINE) {
		biodb$getConfig()$set('cache.directory', OFFLINE.CACHE.DIR)
		biodb$getConfig()$enable('cache.read.only')
		biodb$getConfig()$disable('allow.huge.downloads')
		biodb$getConfig()$enable('offline')
		biodb$getConfig()$disable('cache.subfolders')
	}

	# Unknown mode
	else {
		stop(paste("Unknown mode \"", mode, "\".", sep = "."))
	}
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

	return(ids)
}

# Load ref entry {{{1
################################################################

load.ref.entry <- function(db, id) {

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


# Get default connector {{{1
################################################################

create.conn.for.generic.tests = function(biodb, class.db, mode) {

	# Get connector
	if (biodb$getFactory()$connExists(class.db))
		conn = biodb$getFactory()$getConn(class.db)

	# Create connector
	else {
		if (mode == 'offline')
			conn = biodb$getFactory()$createConn(class.db, conn.id = class.db, cache.id = class.db)
		else
			conn = biodb$getFactory()$createConn(class.db)

		# Set parameters for local connectors
		if (class.db == 'mass.csv.file') {
			conn$setUrl('base.url', MASSFILEDB.URL)
			conn$setField('accession', c('compound.id', 'ms.mode', 'chrom.col.name', 'chrom.rt'))
		}
		else if (class.db == 'mass.sqlite') {
			conn$setUrl('base.url', MASS.SQLITE.URL)

			# Create SQLite database file
			if ( ! file.exists(MASS.SQLITE.URL)) {

				mass.csv.file.conn = create.conn.for.generic.tests(biodb = biodb, class.db = 'mass.csv.file',  mode = mode)
				conn$allowEditing()
				biodb$copyDb(conn.from = mass.csv.file.conn, conn.to = conn)
				conn$allowWriting()
				conn$write()
				conn$disallowWriting()
				conn$disallowEditing()
			}
		}

		# Create needed additional connectors for computing missing fields
		if (mode == 'offline') {

			# Loop on all fields
			for (field in biodb$getEntryFields()$getFieldNames())
				for (computing.db in biodb$getEntryFields()$get(field)$getComputableFrom())
					if (computing.db != class.db)
						c = create.conn.for.generic.tests(biodb = biodb, class.db = computing.db, mode = mode)
		}
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

# Create test observer {{{1
################################################################

create.test.observer <- function(biodb) {

	# Create test observer class
	TestObs <- methods::setRefClass("TestObs", contains = "BiodbObserver", fields = list(.msgs = 'character', .msgs.by.type = 'list'))
	TestObs$methods( initialize = function(...) {
		.msgs <<- character()
		.msgs.by.type <<- list()
	})
	TestObs$methods( message = function(type, msg, class = NA_character_, method = NA_character_, level = 1) {
		.msgs <<- c(.self$.msgs, msg)
		.self$.msgs.by.type[[type]] <- c(.self$.msgs.by.type[[type]], msg)
	})
	TestObs$methods( hasMsgs = function(type = NULL) {

		f = FALSE

		if (is.null(type))
			f = (length(.self$.msg) > 0)
		else
			f = if (type %in% names(.self$.msgs.by.type)) (length(.self$.msgs.by.type[[type]])) else FALSE

		return(f)
	})
	TestObs$methods( lastMsg = function() {
		return(.self$.msgs[[length(.self$.msgs)]])
	})
	TestObs$methods( getLastMsgByType = function(type) {
		m = NULL
		if (type %in% names(.self$.msgs.by.type)) {
			m = .self$.msgs.by.type[[type]]
			m = m[[length(m)]] 
		}
		return(m)
	})
	TestObs$methods( getMsgsByType = function(type) {
		msgs = character()

		if ( ! is.null(type) && type %in% names(.self$.msgs.by.type))
			msgs = .self$.msgs.by.type[[type]]

		return(msgs)
	})
	TestObs$methods( clearMessages = function() {
		.msgs <<- character()
		.msgs.by.type <<- list()
	})
	obs <- TestObs$new()

	# Set observer
	biodb$addObservers(obs)

	return(obs)
}
