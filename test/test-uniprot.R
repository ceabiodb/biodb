#!/usr/bin/env R --slave -f
library(RUnit)
source('../UniProtConn.R', chdir = TRUE)
source('hash-helpers.R', chdir = TRUE)

full_test <- FALSE

entries <- list(
				'Q75MT5' = list(),
                'AAAAAA' = list(false = TRUE)
               )

# Open connexion
conn <- UniProtConn$new(useragent = "fr.cea.test-uniprot ; pierrick.rogermele@cea.fr")

# Loop on all entries
for (id in names(entries)) {

	# Skip big entry (take too much time)
	if (hGetBool(entries[[id]], 'big') && ! full_test)
		next

	print(paste('Testing UniProt entry', id, '...'))

	# Get Entry from database
	entry <- conn$getEntry(id)

	# This is a false entry => test that it's null
	if (hGetBool(entries[[id]], 'false'))
		checkTrue(is.null(entry))

	# This is a real entry => test that it isn't null
	else {
		checkTrue( ! is.null(entry))

		# Check that returned id is the same
		checkEquals(entry$getAccession(), id)

		# Check name
		checkTrue(entry$getName() != '')

		# Check length
		checkTrue(entry$getLength() > 0)

		# Check mass
		checkTrue(entry$getMass() > 0)

		# save
		entry$save(paste('test-uniprot-', id, '.xml', sep=''))
	}
}
