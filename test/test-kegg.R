#!/usr/bin/env Rscript
library(RUnit)
library(getopt)
source('../KeggConn.R', chdir = TRUE)
source('../../r-lib/hshhlp.R', chdir = TRUE)

#############
# CONSTANTS #
#############

ENTRIES <- list(
                list(id = 'hsa:3627'),
                list(id = 'ec:1.1.1.54'),
                list(id = 'BLABLABLA', false = TRUE),
                list(id = 'cpd:C00751')
                )

####################
# ONLINE TEST KEGG #
####################

online.test.kegg <- function() {

	# Open connexion
	conn <- KeggConn$new(useragent = USER.AGENT)

	# Loop on all entries
	for (e in ENTRIES) {

		# Get ID
		if ( ! 'id' %in% names(e))
			stop('One element of ENTRIES does not contain an "id" field.')
		id <- e[['id']]

		print(paste('Testing KEGG entry', id, '...'))

		# Get Entry from database
		entry <- conn$createEntry(conn$downloadEntryFileContent(id, save_as = paste0('test-kegg-', id, '.txt')))

		for (f in names(e))
			switch(f,
			       id = if ( ! 'false' %in% names(e)) checkTrue( ! is.null(entry)) && checkEquals(entry$getId(), id),
			       false = checkTrue(is.null(entry)),
			       inchi = checkEquals(entry$getInchi(), e[['inchi']]),
			       inchikey = checkEquals(entry$getInchiKey(), e[['inchikey']])
			      )
	}
}
