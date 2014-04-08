#!/usr/bin/env R --slave -f
library(RUnit)
source('../LipidmapsConn.R', chdir = TRUE)
source('../../r-lib/hshhlp.R', chdir = TRUE)

full_test <- FALSE
# TODO add a flag for running long tests
#args <- commandArgs(trailingOnly = TRUE)
#full_test = args[1]

entries <- list('LMFA08040013' = list(formula = 'C18H37NO2', mass = 299.28, synonyms = c('Palmitoyl ethanolamide', 'palmitoylethanolamide', 'Anandamide (16:0)', 'N-palmitoyl ethanolamine'), kegg_id = NA_character_, hmdb_id = 'HMDB02100'),
                'TAGADA' = list(false = TRUE)
                )

# Open connexion
conn <- LipidmapsConn$new(useragent = "fr.cea.test-lipidmaps ; pierrick.rogermele@cea.fr")

# Loop on all entries
for (id in names(entries)) {

	# Skip big entry (take too much time)
	if (hGetBool(entries[[id]], 'big') && ! full_test)
		next

	print(paste('Testing LIPIDMAPS entry', id, '...'))

	# Get Entry from database
	entry <- conn$createEntry(conn$downloadEntryFileContent(id, save_as = paste0('test-lipidmaps-', id, '.txt')))

	# This is a false entry => test that it's null
	if (hGetBool(entries[[id]], 'false'))
		checkTrue(is.null(entry))

	# This is a real entry => test that it isn't null
	else {
		checkTrue( ! is.null(entry))

		# Check that returned id is the same
		checkEquals(id, entry$getId())

		# Check synonyms
		if (hHasKey(entries[[id]], 'synonyms'))
			checkEquals(sort(entries[[id]][['synonyms']]), sort(entry$getSynonyms()))
			
		# Check Kegg ID
		if (hHasKey(entries[[id]], 'kegg_id'))
			checkEquals(entries[[id]][['kegg_id']], entry$getKeggId())
			
		# Check HMDB ID
		if (hHasKey(entries[[id]], 'hmdb_id'))
			checkEquals(entries[[id]][['hmdb_id']], entry$getHmdbId())
			
		# Check Mass
		if (hHasKey(entries[[id]], 'mass'))
			checkEquals(entries[[id]][['mass']],entry$getMass())
			
		# Check Formula
		if (hHasKey(entries[[id]], 'formula'))
			checkEquals(entries[[id]][['formula']], entry$getFormula())
	}
}
