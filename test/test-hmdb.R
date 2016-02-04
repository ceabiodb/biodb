#!/usr/bin/env R --slave -f
library(RUnit)
source('../HmdbConn.R', chdir = TRUE)
source('../../r-lib/hshhlp.R', chdir = TRUE)

options(error = function() { traceback(2) ; q(status = 1) }, warn = 2 )

full_test <- FALSE
# TODO add a flag for running long tests
#args <- commandArgs(trailingOnly = TRUE)
#full_test = args[1]

compounds <- list('HMDB00001' = list( keggid = 'C01152' ),
                'TOTO' = list(false = TRUE)
                )

# Open connexion
conn <- HmdbConn$new(useragent = "fr.cea.test-hmdb ; pierrick.rogermele@cea.fr")

# Loop on all compounds
for (id in names(compounds)) {

	# Skip big compound (take too much time)
	if (hGetBool(compounds[[id]], 'big') && ! full_test)
		next

	print(paste('Testing HMDB compound', id, '...'))

	# Get Compound from database
	compound <- conn$createCompound(conn$downloadCompoundFileContent(id, save_as = paste0('test-hmdb-', id, '.xml')))

	# This is a false compound => test that it's null
	if (hGetBool(compounds[[id]], 'false'))
		checkTrue(is.null(compound))

	# This is a real compound => test that it isn't null
	else {
		checkTrue( ! is.null(compound))

		# Check that returned id is the same
		checkEquals(compound$getId(), id)
		
		# Check Kegg ID
		if (hHasKey(compounds[[id]], 'keggid'))
			checkEquals(compound$getKeggId(), compounds[[id]][['keggid']])

		# Check name
		checkTrue(compound$getName() != '')

		# Check formula
		checkTrue(compound$getFormula() != '')

		# Check super class
		checkTrue(compound$getSuperClass() != '')

		# Check average mass
		checkTrue(compound$getAverageMass() > 0)

		# Check monoisotopic mass
		checkTrue(compound$getMonoisotopicMass() > 0)
	}
}
