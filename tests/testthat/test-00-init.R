# vi: fdm=marker

context('Initialize tests.')

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
if ('MODES' %in% names(ENV) && ENV[['MODES']] == MODE.FULL) {
	biodb <- Biodb$new(logger = FALSE)
	cat(paste('Delete whole cache folder ', biodb$getConfig()$get('cache.directory'), ".\n", sep = ''))
	biodb$getCache()$eraseFolder()
}

# Erase only short term cache
if (MODE.ONLINE %in% TEST.MODES) {
	biodb <- Biodb$new(logger = FALSE)
	cat(paste('Delete short term cache folder ', biodb$getCache()$getSubFolderPath('shortterm'), ".\n", sep = ''))
	biodb$getCache()$eraseFolder('shortterm')
}

biodb <- Biodb$new(logger = FALSE)
cat(paste('Using cache folder ', biodb$getConfig()$get('cache.directory'), ".\n", sep = ''))

# Remove all log files {{{1
################################################################

log.files <- Sys.glob(file.path(TEST.DIR, '*.log'))
unlink(log.files)
