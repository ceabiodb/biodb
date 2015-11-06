#!/usr/bin/env Rscript
library(RUnit)
library(getopt)

#############
# READ ARGS #
#############

read_args <- function() {
  
  # program name
  prog <- sub('^.*/([^/]+)$', '\\1', commandArgs()[4], perl = TRUE)
  
  # options
  spec = matrix(c(
    'full',         'f', 0, 'logical',      'Full test. Run all tests (online, long, etc.). Disabled by default.',
    'help',         'h', 0, 'logical',      'Print this help.',
    'long',         'l', 0, 'logical',      'Run long tests. Disabled by default.',
    'online',       'o', 0, 'logical',      'Run online tests. Disabled by default.'
  ), byrow = TRUE, ncol = 5)
   
  opt <- getopt(spec)
  opt$full = if (is.null(opt$full)) FALSE else TRUE
  opt$long = if (is.null(opt$full)) FALSE else TRUE
  opt$online = if (is.null(opt$full)) FALSE else TRUE

  # help
  if ( ! is.null(opt$help)) {
    cat(getopt(spec, usage = TRUE, command = prog))
    q(status = 1)
  }

  return(opt)
}

########
# MAIN #
########

# Call tests matching name: in script name or function name:
#  test.chebi.offline
#  test.chebi.online
#  test.chebi.full or test.chebi.long

options(error = function() { traceback(2) ; q(status = 1) }, warn = 2 )

opt<-read_args()
