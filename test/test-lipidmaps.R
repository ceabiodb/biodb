#!/usr/bin/env R --slave -f
library(RUnit)
source('../LipidmapsConn.R', chdir = TRUE)
source('../../r-lib/hshhlp.R', chdir = TRUE)

options(error = function() { traceback(2) ; q(status = 1) }, warn = 2 )

full_test <- FALSE
# TODO add a flag for running long tests
#args <- commandArgs(trailingOnly = TRUE)
#full_test = args[1]

compounds <- list('LMFA08040013' = list(formula = 'C18H37NO2', mass = 299.28, synonyms = c('Palmitoyl ethanolamide', 'palmitoylethanolamide', 'Anandamide (16:0)', 'N-palmitoyl ethanolamine'), kegg_id = NA_character_, hmdb_id = 'HMDB02100'),
                'TAGADA' = list(false = TRUE)
                )

# Open connexion
conn <- LipidmapsConn$new(useragent = "fr.cea.test-lipidmaps ; pierrick.rogermele@cea.fr")

# Loop on all compounds
for (id in names(compounds)) {

	# Skip big compound (take too much time)
	if (hGetBool(compounds[[id]], 'big') && ! full_test)
		next

	print(paste('Testing LIPIDMAPS compound', id, '...'))

	# Get Compound from database
	compound <- conn$createCompound(conn$downloadCompoundFileContent(id, save_as = paste0('test-lipidmaps-', id, '.txt')))

	# This is a false compound => test that it's null
	if (hGetBool(compounds[[id]], 'false'))
		checkTrue(is.null(compound))

	# This is a real compound => test that it isn't null
	else {
		checkTrue( ! is.null(compound))

		# Check that returned id is the same
		checkEquals(id, compound$getId())

		# Check synonyms
		if (hHasKey(compounds[[id]], 'synonyms'))
			checkEquals(sort(compounds[[id]][['synonyms']]), sort(compound$getSynonyms()))
			
		# Check Kegg ID
		if (hHasKey(compounds[[id]], 'kegg_id'))
			checkEquals(compounds[[id]][['kegg_id']], compound$getKeggId())
			
		# Check HMDB ID
		if (hHasKey(compounds[[id]], 'hmdb_id'))
			checkEquals(compounds[[id]][['hmdb_id']], compound$getHmdbId())
			
		# Check Mass
		if (hHasKey(compounds[[id]], 'mass'))
			checkEquals(compounds[[id]][['mass']],compound$getMass())
			
		# Check Formula
		if (hHasKey(compounds[[id]], 'formula'))
			checkEquals(compounds[[id]][['formula']], compound$getFormula())
	}
}
