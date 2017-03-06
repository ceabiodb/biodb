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
if ('DATABASES' %in% names(env)) {
	TEST.DATABASES <- env[['DATABASES']]
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
if ('MODES' %in% names(env)) {
	if (env[['MODES']] == MODE.ALL)
		TEST.MODES <- ALLOWED.MODES
	else {
		mode.exists <- env[['MODES']] %in% ALLOWED.MODES
		if ( ! all(mode.exists)) {
			wrong.modes <- env[['MODES']][ ! mode.exists]
			stop(paste('Unknown testing mode(s) ', paste(wrong.modes, collapse = ', ')), '.', sep = '')
		}
		TEST.MODES <- env[['MODES']]
	}
} else {
	TEST.MODES <- DEFAULT.MODES
}
