# vi: fdm=marker

# Constants {{{1
################################################################

TEST.DIR <- file.path(getwd(), '..')
OUTPUT.DIR <- file.path(TEST.DIR, 'output')
RES.DIR  <- file.path(TEST.DIR, 'res')
REF.FILES.DIR <- file.path(RES.DIR, 'ref-files')
OFFLINE.CACHE.DIR <- file.path(RES.DIR, 'offline-cache')
ONLINE.CACHE.DIR <- file.path(TEST.DIR, 'cache')
LOG.FILE.PATH <- file.path(TEST.DIR, 'test.log')
USERAGENT <- 'biodb.test ; pierrick.rogermele@icloud.com'

MASSFILEDB.URL <- file.path(RES.DIR, 'mass.csv.file.tsv')

# Create output directory
if ( ! file.exists(OUTPUT.DIR))
	dir.create(OUTPUT.DIR)

# Set databases to test {{{1
################################################################

env <- Sys.getenv()
TEST.DATABASES <- biodb::BIODB.DATABASES
if ('DATABASES' %in% names(env) && nchar(env[['DATABASES']]) > 0) {
	if (env[['DATABASES']] == 'none')
		TEST.DATABASES <- character(0)
	else {
		TEST.DATABASES <- strsplit(env[['DATABASES']], ',')[[1]]
		db.exists <- TEST.DATABASES %in% BIODB.DATABASES
		if ( ! all(db.exists)) {
			wrong.dbs <- TEST.DATABASES[ ! db.exists]
			stop(paste('Unknown testing database(s) ', paste(wrong.dbs, collapse = ', ')), '.', sep = '')
		}
	}
}

# Define modes {{{1
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

# Create Biodb instance {{{1
################################################################

create.biodb.instance <- function() {

	# Create instance
	biodb <- Biodb$new(logger = FALSE, observers = BiodbLogger$new(file = LOG.FILE.PATH, mode = 'a'))

	# Set user agent
	biodb$getConfig()$set(CFG.USERAGENT, USERAGENT)

	return(biodb)
}
# Set test context {{{1
################################################################

set.test.context <- function(biodb, text) {

	# Set testthat context
	context(text)

	# Print banner in log file
	biodb$message(MSG.INFO, "")
	biodb$message(MSG.INFO, "****************************************************************")
	biodb$message(MSG.INFO, paste("Test context", text, sep = " - "))
	biodb$message(MSG.INFO, "****************************************************************")
	biodb$message(MSG.INFO, "")
}

# Set mode {{{1
################################################################

set.mode <- function(biodb, mode) {

	# Online
	if (mode == MODE.ONLINE) {
		biodb$getConfig()$set(CFG.CACHE.DIRECTORY, ONLINE.CACHE.DIR)
		biodb$getConfig()$disable(CFG.CACHE.READ.ONLY)
		biodb$getConfig()$enable(CFG.ALLOW.HUGE.DOWNLOADS)
		biodb$getConfig()$disable(CFG.OFFLINE)
		biodb$getConfig()$enable(CFG.USE.CACHE.SUBFOLDERS)
	}

	# Quick online
	else if (mode == MODE.QUICK.ONLINE) {
		biodb$getConfig()$set(CFG.CACHE.DIRECTORY, ONLINE.CACHE.DIR)
		biodb$getConfig()$disable(CFG.CACHE.READ.ONLY)
		biodb$getConfig()$disable(CFG.ALLOW.HUGE.DOWNLOADS)
		biodb$getConfig()$disable(CFG.OFFLINE)
		biodb$getConfig()$enable(CFG.USE.CACHE.SUBFOLDERS)
	}

	# Offline
	else if (mode == MODE.OFFLINE) {
		biodb$getConfig()$set(CFG.CACHE.DIRECTORY, OFFLINE.CACHE.DIR)
		biodb$getConfig()$enable(CFG.CACHE.READ.ONLY)
		biodb$getConfig()$disable(CFG.ALLOW.HUGE.DOWNLOADS)
		biodb$getConfig()$enable(CFG.OFFLINE)
		biodb$getConfig()$disable(CFG.USE.CACHE.SUBFOLDERS)
	}

	# Unknown mode
	else {
		stop(paste("Unknown mode \"", mode, "\".", sep = "."))
	}
}

# Load reference entries {{{1
################################################################

load.ref.entries <- function(db) {

	# Define reference file
	entries.file <- file.path(RES.DIR, paste0(db, '-entries.txt'))
	expect_true(file.exists(entries.file), info = paste0("Cannot find file \"", entries.file, "\"."))

	# Load reference contents from file
	entries.desc <- read.table(entries.file, stringsAsFactors = FALSE, header = TRUE)
	expect_true(nrow(entries.desc) > 0, info = paste0("No reference entries found in file \"", entries.file, "\" in test.entry.fields()."))

	return(entries.desc)
}

# Initialize MassCsvFile db {{{1
################################################################

init.mass.csv.file.db <- function(biodb) {
	db.instance <- biodb$getFactory()$createConn(BIODB.MASS.CSV.FILE, url = MASSFILEDB.URL)
	db.instance$setField(BIODB.ACCESSION, c('compoundid', 'msmode', 'chromcol', 'chromcolrt'))
}
