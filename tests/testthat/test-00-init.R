# vi: fdm=marker

context('Initialize tests.')

source('common.R')

# Remove cache folder {{{1
################################################################

# Erase whole cache
if ('MODES' %in% names(ENV) && ENV[['MODES']] == MODE.FULL) {
	biodb <- Biodb$new(logger = FALSE)
	cat(paste('Delete whole cache folder ', biodb$getConfig()$get('cache.directory'), ".\n", sep = ''))
	biodb$getCache()$eraseFolder()
}

## Erase only short term cache
#if (MODE.ONLINE %in% TEST.MODES) {
#	biodb <- Biodb$new(logger = FALSE)
#	biodb$getCache()$eraseFolder('shortterm')
#}

#biodb <- Biodb$new(logger = FALSE)

# Remove all log files {{{1
################################################################

log.files <- Sys.glob(file.path(TEST.DIR, '*.log'))
unlink(log.files)
