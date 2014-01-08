#!/usr/bin/env R --slave -f
library(RUnit)
source('../UniProtConn.R', chdir = TRUE)
source('hash-helpers.R', chdir = TRUE)

full_test <- FALSE

entries <- list(
				'Q75MT5' = list(),
                'AAAAAA' = list(false = TRUE),
				'Q31611' = list(fullname='B2 microglobulin', gene_symbol='HLA-G2.2'),
				'P07911' = list(fullname='Uromodulin', gene_symbol='UMOD'),
				'P08123' = list(fullname='Collagen alpha-2(I) chain', gene_symbol='COL1A2'),
				'P02461' = list(fullname='Collagen alpha-1(III) chain', gene_symbol='COL3A1'),
				'P02462' = list(fullname='Collagen alpha-1(IV) chain', gene_symbol='COL4A1'),
				'P09237' = list(fullname='Matrilysin', gene_symbol='MMP7'),
				'P60022' = list(fullname='Beta-defensin 1', gene_symbol='DEFB1'),
				'P01011' = list(fullname='Alpha-1-antichymotrypsin', gene_symbol='SERPINA3')
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

		entry$save(paste('test-uniprot-', id, '.xml', sep=''))

		# Check that returned id is the same
		checkEquals(entry$getId(), id)

		# Check name
		checkTrue(entry$getName() != '')

		# Check fullname
		if (hHasKey(entries[[id]], 'fullname'))
			checkEquals(entry$getFullName(), entries[[id]][['fullname']])

		# Check gene symbol
		if (hHasKey(entries[[id]], 'gene_symbol'))
			checkEquals(entry$getGeneSymbol(), entries[[id]][['gene_symbol']])

		# Check length
		checkTrue(entry$getLength() > 0)

		# Check mass
		checkTrue(entry$getMass() > 0)

		# save
		entry$save(paste('test-uniprot-', id, '.xml', sep=''))
	}
}
