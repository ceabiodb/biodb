#!/usr/bin/env Rscript
# vi: ft=R fdm=marker

for (lib in c('getopt', 'R.utils')) {
	library(lib, character.only = TRUE, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
	detach(paste('package', lib, sep = ':'), character.only = TRUE, unload = TRUE)
}

# CONSTANTS {{{1
################################################################

args <- commandArgs(trailingOnly = F)
SCRIPT.PATH <- sub("--file=","",args[grep("--file=",args)])
SCRIPT.DIR <- dirname(SCRIPT.PATH)
if ( ! R.utils::isAbsolutePath(SCRIPT.DIR)) SCRIPT.DIR <- file.path(getwd(), SCRIPT.DIR)

USER.AGENT <- "biodb.test ; pierrick.roger@gmail.com"

# READ ARGS {{{1
################################################################

read_args <- function() {

	# options
	spec <- c(
		'databases',    'd',    1,  'character',    'Set list of databases on which to run the tests. Optional.',
		'help',         'h',    0,  'logical',      'Print this help.',
		'online',       'o',    1,  'character',    'Enable or disable online testing.'
		)
	spec <- matrix(spec, byrow = TRUE, ncol = 5)
	opt <- getopt::getopt(spec)

	# Parse option values
	if ( ! is.null(opt$online))
		opt[['online']] <- as.logical(opt$online)
	if ( ! is.null(opt$databases))
		opt[['databases']] <- strsplit(opt[['databases']], ',')

	# help
	if ( ! is.null(opt$help))
		print.help(spec)

	return(opt)
}

# MAIN {{{1
################################################################

# Read command line arguments
opt <- read_args()

# Run tests
devtools::test(SCRIPT.DIR)
