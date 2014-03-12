#!/usr/bin/env Rscript
library(RUnit)
library(getopt)
source('../EnzymeConn.R', chdir = TRUE)
source('hash-helpers.R', chdir = TRUE)

####################
# GLOBAL CONSTANTS #
####################

ENTRIES <- list(
                '1.1.1.1' = list( keggid = 'ec:1.1.1.1' ),
                '1.1.1.54' = list( keggid = 'ec:1.1.1.54' ),
                'BLABLABLA' = list(false = TRUE)
                )

#############
# READ ARGS #
#############

read_args <- function() {
  
  # program name
  prog <- sub('^.*/([^/]+)$', '\\1', commandArgs()[4], perl = TRUE)
  
  # options
  spec = matrix(c(
    'full',         'f', 0, 'logical',      'Full test. Disabled by default.',
    'help',         'h', 0, 'logical',      'Print this help.'
  ), byrow = TRUE, ncol = 5)
   
  opt <- getopt(spec)
  opt$full = if (is.null(opt$full)) FALSE else TRUE

  # help
  if ( ! is.null(opt$help)) {
    cat(getopt(spec, usage = TRUE, command = prog))
    q(status = 1)
  }

  return(opt)
}

################
# TEST ENTRIES #
################

test_entries <- function(conn, entries, full_test = FALSE) {

# Loop on all entries
	for (id in names(entries)) {

		# Skip big entry (take too much time)
		if (hGetBool(entries[[id]], 'big') && ! full_test)
			next

		print(paste('Testing ENZYME entry', id, '...'))

		# Get Entry from database
		entry <- conn$getEntry(id)

		# This is a false entry => test that it's null
		if (hGetBool(entries[[id]], 'false'))
			checkTrue(is.null(entry))

		# This is a real entry => test that it isn't null
		else {
			checkTrue( ! is.null(entry))

			# save
			entry$save(paste('test-enzyme-', id, '.txt', sep=''))

			# Check that returned id is the same
			checkEquals(entry$getId(), id)

			# Check that a description exists
			checkTrue(entry$getDescription() != "")
			
			# Check Kegg ID
			if (hHasKey(entries[[id]], 'keggid'))
				checkEquals(entry$getKeggId(), entries[[id]][['keggid']])
		}
	}
}

########
# MAIN #
########
opt<-read_args()
conn <- EnzymeConn$new(useragent = "fr.cea.r-biodb.test-enzyme ; pierrick.rogermele@cea.fr")
test_entries(conn, ENTRIES, full_test = opt$full)
