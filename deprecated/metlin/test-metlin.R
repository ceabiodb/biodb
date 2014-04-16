#!/usr/bin/env R --slave -f
library(RUnit)
source('../MetlinConn.R', chdir = TRUE)
source('../../r-lib/hshhlp.R', chdir = TRUE)

entries <- list('66045' = list(),
                'blabla' = list( false = TRUE )
                )

# Open connexion
conn <- MetlinConn$new(useragent = "fr.cea.test-metlin ; pierrick.rogermele@cea.fr")

# Loop on all entries
for (id in names(entries)) {

	print(paste('Testing METLIN entry', id, '...'))

	# Get Entry from database
	entry <- conn$createEntry(conn$downloadEntryFileContent(id, save_as = paste0('test-metlin-', id, '.html')))

	# This is a false entry => test that it's null
	if (hGetBool(entries[[id]], 'false'))
		checkTrue(is.null(entry))

	# This is a real entry => test that it isn't null
	else {
		checkTrue( ! is.null(entry))

		# Check that returned id is the same
		checkEquals(id, entry$getId())
	}
}

