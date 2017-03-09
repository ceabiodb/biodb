# vi: fdm=marker

# Constants {{{1
################################################################

TEST.DIR <- file.path(getwd(), '..')
RES.DIR  <- file.path(TEST.DIR, 'res')
OFFLINE.FILES.DIR <- file.path(RES.DIR, 'offline-files')
CACHE.DIR <- file.path(TEST.DIR, 'cache')
LOG.DIR  <- file.path(TEST.DIR)
LOG.FILE <- file.path(LOG.DIR, 'test.log')
USERAGENT <- 'biodb.test ; pierrick.rogermele@cloud.com'

# Set databases to test {{{1
################################################################

env <- Sys.getenv()
if ('DATABASES' %in% names(env) && nchar(env[['DATABASES']]) > 0) {
	TEST.DATABASES <- strsplit(env[['DATABASES']], ',')[[1]]
	db.exists <- TEST.DATABASES %in% BIODB.DATABASES
	if ( ! all(db.exists)) {
		wrong.dbs <- TEST.DATABASES[ ! db.exists]
		stop(paste('Unknown testing database(s) ', paste(wrong.dbs, collapse = ', ')), '.', sep = '')
	}
} else {
	TEST.DATABASES <- BIODB.DATABASES
}

# Set modes {{{1
################################################################

MODE.OFFLINE <- 'offline'
MODE.ONLINE <- 'online'
MODE.QUICK.ONLINE <- 'quick.online'
MODE.ALL <- 'all'
DEFAULT.MODES <- MODE.OFFLINE
ALLOWED.MODES <- c(MODE.OFFLINE, MODE.QUICK.ONLINE, MODE.ONLINE)
if ('MODES' %in% names(env) && nchar(env[['MODES']]) > 0) {
	if (env[['MODES']] == MODE.ALL)
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
	biodb <- Biodb$new(logger = FALSE, observers = BiodbLogger$new(file = LOG.FILE, mode = 'a'))

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
		biodb$getConfig()$set(CFG.CACHE.DIRECTORY, CACHE.DIR)
		biodb$getConfig()$disable(CFG.CACHE.READ.ONLY)
		biodb$getConfig()$enable(CFG.ALLOW.HUGE.DOWNLOADS)
		biodb$getConfig()$disable(CFG.OFFLINE)
	}

	# Quick online
	else if (mode == MODE.QUICK.ONLINE) {
		biodb$getConfig()$set(CFG.CACHE.DIRECTORY, CACHE.DIR)
		biodb$getConfig()$disable(CFG.CACHE.READ.ONLY)
		biodb$getConfig()$disable(CFG.ALLOW.HUGE.DOWNLOADS)
		biodb$getConfig()$disable(CFG.OFFLINE)
	}

	# Offline
	else if (mode == MODE.OFFLINE) {
		biodb$getConfig()$set(CFG.CACHE.DIRECTORY, OFFLINE.FILES.DIR)
		biodb$getConfig()$enable(CFG.CACHE.READ.ONLY)
		biodb$getConfig()$disable(CFG.ALLOW.HUGE.DOWNLOADS)
		biodb$getConfig()$enable(CFG.OFFLINE)
	}

	# Unknown mode
	else {
		stop(paste("Unknown mode \"", mode, "\".", sep = "."))
	}
}
