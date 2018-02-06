# vi: fdm=marker

# Constants {{{1
################################################################

TEST.DIR <- file.path(getwd(), '..')
OUTPUT.DIR <- file.path(TEST.DIR, 'output')
RES.DIR  <- file.path(TEST.DIR, 'res')
REF.FILES.DIR <- file.path(RES.DIR, 'ref-files')
OFFLINE.CACHE.DIR <- file.path(RES.DIR, 'offline-cache')
#ONLINE.CACHE.DIR <- file.path(TEST.DIR, 'cache')
LOG.FILE.PATH <- file.path(TEST.DIR, 'test.log')
USERAGENT <- 'biodb.test ; pk.roger@icloud.com'

MASSFILEDB.URL <- file.path(RES.DIR, 'mass.csv.file.tsv')
MASSFILEDB.WRONG.HEADER.URL <- file.path(RES.DIR, 'mass.csv.file-wrong_header.tsv')
MASSFILEDB.WRONG.NB.COLS.URL <- file.path(RES.DIR, 'mass.csv.file-wrong_nb_cols.tsv')

# Create output directory
if ( ! file.exists(OUTPUT.DIR))
	dir.create(OUTPUT.DIR)

# Set databases to test {{{1
################################################################

DATABASES.ALL <- 'all'
DATABASES.NONE <- 'none'

env <- Sys.getenv()
TEST.DATABASES <- biodb::Biodb$new(logger = FALSE)$getDbsInfo()$getIds()
if ('DATABASES' %in% names(env) && nchar(env[['DATABASES']]) > 0) {
	if (env[['DATABASES']] == DATABASES.NONE)
		TEST.DATABASES <- character(0)
	else if (env[['DATABASES']] == DATABASES.ALL)
		TEST.DATABASES <- biodb::Biodb$new(logger = FALSE)$getDbsInfo()$getIds()
	else {
		TEST.DATABASES <- strsplit(env[['DATABASES']], ',')[[1]]
		db.exists <- vapply(TEST.DATABASES, function(x) biodb::Biodb$new(logger = FALSE)$getDbsInfo()$isDefined(x), FUN.VALUE = TRUE)
		if ( ! all(db.exists)) {
			wrong.dbs <- TEST.DATABASES[ ! db.exists]
			stop(paste('Unknown testing database(s) ', paste(wrong.dbs, collapse = ', ')), '.', sep = '')
		}
	}
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
if ('MODES' %in% names(env) && nchar(env[['MODES']]) > 0) {
	if (env[['MODES']] %in% c(MODE.ALL, MODE.FULL))
		TEST.MODES <- ALLOWED.MODES
	else {
		TEST.MODES <- strsplit(env[['MODES']], ',')[[1]]
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
if ('FUNCTIONS' %in% names(env)) {
	TEST.FUNCTIONS <- strsplit(env[['FUNCTIONS']], ',')[[1]]
} else {
	TEST.FUNCTIONS <- FUNCTION.ALL
}

# Create Biodb instance {{{1
################################################################

create.biodb.instance <- function() {

	# Create logger
	logger = BiodbLogger$new(file = LOG.FILE.PATH, mode = 'a')
	logger$includeMsgType('debug')

	# Create instance
	biodb <- Biodb$new(logger = FALSE, observers = logger)

	# Set user agent
	biodb$getConfig()$set('useragent', USERAGENT)

	# Set Peakforest URL and token
	if ('BIODB_TEST_PEAKFOREST_TOKEN' %in% names(env))
		for (db in c('peakforest.mass', 'peakforest.compound'))
			biodb$getDbsInfo()$get(db)$setToken(env[['BIODB_PEAKFOREST_ALPHA_TOKEN']])
	if ('BIODB_TEST_PEAKFOREST_URL' %in% names(env))
		for (db in c('peakforest.mass', 'peakforest.compound'))
			biodb$getDbsInfo()$get(db)$setBaseUrl(env[['BIODB_TEST_PEAKFOREST_URL']])

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
		#biodb$getConfig()$set('cache.directory', ONLINE.CACHE.DIR)
		biodb$getConfig()$disable('cache.read.only')
		biodb$getConfig()$enable('allow.huge.downloads')
		biodb$getConfig()$disable('offline')
		biodb$getConfig()$enable('cache.subfolders')
	}

	# Quick online
	else if (mode == MODE.QUICK.ONLINE) {
		#biodb$getConfig()$set('cache.directory', ONLINE.CACHE.DIR)
		biodb$getConfig()$disable('cache.read.only')
		biodb$getConfig()$disable('allow.huge.downloads')
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

# Initialize MassCsvFile db {{{1
################################################################

init.mass.csv.file.db <- function(biodb) {
	db.instance <- biodb$getFactory()$createConn('mass.csv.file', url = MASSFILEDB.URL)
	db.instance$setField('accession', c('compound.id', 'ms.mode', 'chrom.col.name', 'chrom.rt'))
	db.instance$addField('chrom.rt.unit', 's')
	return(db.instance)
}

# Run database test that {{{1
################################################################

run.db.test.that <- function(msg, fct, db) {
	if (TEST.FUNCTIONS == FUNCTION.ALL || fct %in% TEST.FUNCTIONS)
		test_that(msg, do.call(fct, list(db)))
}

# Create test observer {{{1
################################################################

create.test.observer <- function(biodb) {

	# Create test observer class
	TestObs <- methods::setRefClass("TestObs", contains = "BiodbObserver", fields = list(msgs = 'character'))
	TestObs$methods( initialize = function(...) {
		msgs <<- character(0)
	})
	TestObs$methods( message = function(type, msg, class = NA_character_, method = NA_character_, level = 1) {
		msgs <<- c(.self$msgs, msg)
	})
	TestObs$methods( lastMsg = function() {
		return(.self$msgs[[length(.self$msgs)]])
	})
	obs <- TestObs$new()

	# Set observer
	biodb$addObservers(obs)

	return(obs)
}
