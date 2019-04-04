# vi: fdm=marker

context('Initialize tests.')

source('common.R')

# Remove all log files {{{1
################################################################

log.files = Sys.glob(file.path(TEST.DIR, '*.log'))
unlink(log.files)
