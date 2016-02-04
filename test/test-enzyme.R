#!/usr/bin/env Rscript
library(RUnit)
library(getopt)
source('../EnzymeConn.R', chdir = TRUE)
source('../../r-lib/hshhlp.R', chdir = TRUE)

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

test_compounds <- function(conn, compounds, full_test = FALSE) {

# Loop on all compounds
	for (id in names(compounds)) {

		# Skip big compound (take too much time)
		if (hGetBool(compounds[[id]], 'big') && ! full_test)
			next

		print(paste('Testing ENZYME compound', id, '...'))

		# Get Compound from database
		compound <- conn$createCompound(conn$downloadCompoundFileContent(id, save_as = paste0('test-enzyme-', id, '.txt')))

		# This is a false compound => test that it's null
		if (hGetBool(compounds[[id]], 'false'))
			checkTrue(is.null(compound))

		# This is a real compound => test that it isn't null
		else {
			checkTrue( ! is.null(compound))

			# Check that returned id is the same
			checkEquals(compound$getId(), id)

			# Check that a description exists
			checkTrue(compound$getDescription() != "")
			
			# Check Kegg ID
			if (hHasKey(compounds[[id]], 'keggid'))
				checkEquals(compound$getKeggId(), compounds[[id]][['keggid']])
		}
	}
}

########
# MAIN #
########

options(error = function() { traceback(2) ; q(status = 1) }, warn = 2 )

opt<-read_args()
conn <- EnzymeConn$new(useragent = "fr.cea.r-biodb.test-enzyme ; pierrick.rogermele@cea.fr")
test_compounds(conn, ENTRIES, full_test = opt$full)
