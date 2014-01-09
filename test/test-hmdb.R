#!/usr/bin/env R --slave -f
library(RUnit)
source('../HmdbConn.R', chdir = TRUE)
source('hash-helpers.R', chdir = TRUE)

full_test <- FALSE
# TODO add a flag for running long tests
#args <- commandArgs(trailingOnly = TRUE)
#full_test = args[1]

entries <- list('HMDB00001' = list(),
                'TOTO' = list(false = TRUE)
                )

# Open connexion
conn <- HmdbConn$new(useragent = "fr.cea.test-hmdb ; pierrick.rogermele@cea.fr")

# Loop on all entries
for (id in names(entries)) {

	# Skip big entry (take too much time)
	if (hGetBool(entries[[id]], 'big') && ! full_test)
		next

	print(paste('Testing HMDB entry', id, '...'))

	# Get Entry from database
	entry <- conn$getEntry(id)

	# This is a false entry => test that it's null
	if (hGetBool(entries[[id]], 'false'))
		checkTrue(is.null(entry))

	# This is a real entry => test that it isn't null
	else {
		checkTrue( ! is.null(entry))

		# Check that returned id is the same
		checkEquals(entry$getId(), id)

		# Check name
		checkTrue(entry$getName() != '')

		# Check formula
		checkTrue(entry$getFormula() != '')

		# Check super class
		checkTrue(entry$getSuperClass() != '')

		# Check average mass
		checkTrue(entry$getAverageMass() > 0)

		# Check monoisotopic mass
		checkTrue(entry$getMonoisotopicMass() > 0)

		# save
		entry$save(paste('test-hmdb-', id, '.xml', sep=''))
	}
}
