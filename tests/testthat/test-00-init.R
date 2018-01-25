# vi: fdm=marker

# INITIALIZATION OF TESTS

source('common.R')

# Display information {{{1
################################################################

# Print modes info
cat(paste('Running tests in ', paste(TEST.MODES, collapse = ', '), " mode(s).\n", sep = ''))

# Print databases info
cat(paste('Running tests on database(s) ', paste(TEST.DATABASES, collapse = ', '), ".\n", sep = ''))

# Remove cache folder {{{1
################################################################

# Erase whole cache
if ('MODES' %in% names(env) && env[['MODES']] == MODE.FULL) {
	biodb <- Biodb$new(logger = FALSE)
	#biodb$getConfig()$set('cache.directory', ONLINE.CACHE.DIR)
	biodb$getCache()$eraseFolder()
}

# Erase only short term cache
else if (MODE.ONLINE %in% TEST.MODES) {
	biodb <- Biodb$new(logger = FALSE)
	#biodb$getConfig()$set('cache.directory', ONLINE.CACHE.DIR)
	biodb$getCache()$eraseFolder('shortterm')
}

# Remove log file {{{1
################################################################

if (file.exists(LOG.FILE.PATH)) {
	cat(paste('Delete log file \"', LOG.FILE.PATH, "\".\n", sep = ''))
	unlink(LOG.FILE.PATH)
}
