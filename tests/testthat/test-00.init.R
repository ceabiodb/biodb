# vi: fdm=marker

source('common.R')

# Initialization of tests {{{1
################################################################

# Remove cache folder
if (file.exists(CACHE.DIR)) {

	# Erase whole cache
	if ('MODES' %in% names(env) && env[['MODES']] == MODE.FULL) {
		context(paste('Delete whole cache directory \"', CACHE.DIR, '\".', sep = ''))
		unlink(CACHE.DIR, recursive = TRUE)
	}

	# Erase only short term cache
	else {
		biodb <- biodb:::Biodb$new(logger = FALSE)
		biodb$getConfig()$set(CFG.CACHE.DIRECTORY, CACHE.DIR)
		shortterm.folder <- biodb$getCache()$getFolderPath(biodb:::CACHE.SHORT.TERM.FOLDER)
		context(paste('Delete short term cache folder \"', shortterm.folder, '\".', sep = ''))
		unlink(shortterm.folder, recursive = TRUE)
	}
}

# Remove log file
if (file.exists(LOG.FILE)) {
	context(paste('Delete log file \"', LOG.FILE, '\".', sep = ''))
	unlink(LOG.FILE)
}
