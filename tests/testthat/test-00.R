# vi: fdm=marker

source('init.R')

# Initialization of tests {{{1
################################################################

# Remove cache folder
if (file.exists(CACHE.DIR)) {
	context(paste('Delete cache directory \"', CACHE.DIR, '\".', sep = ''))
	unlink(CACHE.DIR, recursive = TRUE)
}

# Remove log file
if (file.exists(LOG.FILE)) {
	context(paste('Delete log file \"', LOG.FILE, '\".', sep = ''))
	unlink(LOG.FILE)
}
