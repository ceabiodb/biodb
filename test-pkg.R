#!/usr/bin/env Rscript
# vi: ft=R fdm=marker

library(methods)

for (lib in c('getopt', 'R.utils')) {
	library(lib, character.only = TRUE, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
	detach(paste('package', lib, sep = ':'), character.only = TRUE, unload = TRUE)
}

# Constants {{{1
################################################################

args <- commandArgs(trailingOnly = F)
SCRIPT.PATH <- sub("--file=","",args[grep("--file=",args)])
SCRIPT.DIR <- dirname(SCRIPT.PATH)
if ( ! R.utils::isAbsolutePath(SCRIPT.DIR)) SCRIPT.DIR <- file.path(getwd(), SCRIPT.DIR)

MODE.OFFLINE <- 'offline'
MODE.ONLINE <- 'online'
MODE.QUICK.ONLINE <- 'quick.online'
ALLOWED.MODES <- c(MODE.OFFLINE, MODE.ONLINE, MODE.QUICK.ONLINE)
TEST.MODES <- logical()
TEST.DATABASES <- character()
LOG.FILE.PATH <- file.path(SCRIPT.DIR, 'tests', 'test.log')
CACHE.DIR <- file.path(SCRIPT.DIR, 'tests', 'cache')
OFFLINE.FILES.DIR <- file.path(SCRIPT.DIR, 'tests', 'res', 'offline-files')

# Read args {{{1
################################################################

read_args <- function() {

	# options
	spec <- c(
		'databases',       'd',    1,  'character',    'Set list of databases on which to run the tests. Optional.',
		'help',            'h',    0,  'logical',      'Print this help.',
		'modes',           'm',    1,  'character',    'Set list of testing modes. Optional.'
		)
	spec <- matrix(spec, byrow = TRUE, ncol = 5)
	opt <- getopt::getopt(spec)

	# Parse option values
	if ( ! is.null(opt$databases))
		opt[['databases']] <- strsplit(opt[['databases']], ',')
	if ( ! is.null(opt$modes))
		opt[['modes']] <- strsplit(opt[['modes']], ',')

	# help
	if ( ! is.null(opt$help))
		print.help(spec)

	return(opt)
}

# MAIN {{{1
################################################################

# Read command line arguments
opt <- read_args()

# Set testing modes
TEST.MODES <- ALLOWED.MODES
if ( ! is.null(opt[['modes']])) {
	mode.exists <- opt[['modes']] %in% ALLOWED.MODES
	if ( ! all(mode.exists)) {
		wrong.modes <- opt[['modes']][ ! mode.exists]
		stop(paste('Unknown testing mode(s) ', paste(wrong.modes, collapse = ', ')), '.', sep = '')
	}
	TEST.MODES <- opt[['modes']]
}

# Set list of databases to test
if ( ! is.null(opt[['databases']]))
	TEST.DATABASES <- opt[['databases']]

# Create log file
LOG.FILE <- file(LOG.FILE.PATH, open = 'w')

# Run tests
devtools::test(SCRIPT.DIR)

# Close log file
close(LOG.FILE)

# Print warnings
warnings()
