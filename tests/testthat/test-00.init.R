# vi: fdm=marker

source('common.R')

# Initialization of tests {{{1
################################################################

# Remove cache folder
if (file.exists(ONLINE.CACHE.DIR)) {

	# Erase whole cache
	if ('MODES' %in% names(env) && env[['MODES']] == MODE.FULL) {
		cat(paste('Delete whole cache directory \"', ONLINE.CACHE.DIR, "\".\n", sep = ''))
		unlink(ONLINE.CACHE.DIR, recursive = TRUE)
	}

	# Erase only short term cache
	else if ('MODES' %in% names(env) && env[['MODES']] == MODE.ONLINE) {
		biodb <- Biodb$new(logger = FALSE)
		biodb$getConfig()$set(CFG.CACHE.DIRECTORY, ONLINE.CACHE.DIR)
		shortterm.folder <- biodb$getCache()$getFolderPath(CACHE.SHORT.TERM.FOLDER)
		cat(paste('Delete short term cache folder \"', shortterm.folder, "\".\n", sep = ''))
		unlink(shortterm.folder, recursive = TRUE)
	}
}

# Remove log file
if (file.exists(LOG.FILE)) {
	cat(paste('Delete log file \"', LOG.FILE, "\".\n", sep = ''))
	unlink(LOG.FILE)
}
