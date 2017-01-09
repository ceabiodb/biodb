#!/usr/bin/env Rscript
# vi: ft=R fdm=marker

library(getopt)

# CONSTANTS {{{1
################################################################

args <- commandArgs(trailingOnly = F)
SCRIPT.PATH <- sub("--file=","",args[grep("--file=",args)])
SCRIPT.DIR <- dirname(SCRIPT.PATH)

USER.AGENT <- "biodb.test ; pierrick.roger@gmail.com"

# READ ARGS {{{1
################################################################

read_args <- function() {

	# options
	spec <- c(
		'help',     'h',    0,  'logical',        'Print this help.',
		'online',   'o',    1,  'character',      'Enable or disable online testing.'
		)
	spec <- matrix(spec, byrow = TRUE, ncol = 5)
	opt <- getopt(spec)

	# Parse option values
	if (is.null(opt$online))
		opt[['online']] <- TRUE
	else
		opt[['online']] <- as.logical(opt$online)

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
