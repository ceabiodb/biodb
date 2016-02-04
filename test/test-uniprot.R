#!/usr/bin/env R --slave -f
library(RUnit)
source('../UniProtConn.R', chdir = TRUE)
source('../../r-lib/hshhlp.R', chdir = TRUE)

options(error = function() { traceback(2) ; q(status = 1) }, warn = 2 )

full_test <- FALSE

compounds <- list(
				'Q75MT5' = list(),
				'P10480' = list( enzymeid = '2.3.1.43' ),
                'AAAAAA' = list(false = TRUE),
				'Q31611' = list(fullname='B2 microglobulin', gene_symbol='HLA-G2.2'),
				'P07911' = list(fullname='Uromodulin', gene_symbol='UMOD'),
				'P08123' = list(fullname='Collagen alpha-2(I) chain', gene_symbol='COL1A2'),
				'P02461' = list(fullname='Collagen alpha-1(III) chain', gene_symbol='COL3A1'),
				'P02462' = list(fullname='Collagen alpha-1(IV) chain', gene_symbol='COL4A1'),
				'P09237' = list(fullname='Matrilysin', gene_symbol='MMP7'),
				'P60022' = list(fullname='Beta-defensin 1', gene_symbol='DEFB1', seq='MRTSYLLLFTLCLLLSEMASGGNFLTGLGHRSDHYNCVSSGGQCLYSACPIFTKIQGTCYRGKAKCCK'),
				'P01011' = list(fullname='Alpha-1-antichymotrypsin', gene_symbol='SERPINA3', keggid="hsa:12", enzymeid = NA_character_, geneid = '12')
               )

# Open connexion
conn <- UniProtConn$new(useragent = "fr.cea.test-uniprot ; pierrick.rogermele@cea.fr")

# Loop on all compounds
for (id in names(compounds)) {

	# Skip big compound (take too much time)
	if (hGetBool(compounds[[id]], 'big') && ! full_test)
		next

	print(paste('Testing UniProt compound', id, '...'))

	# Get Compound from database
	compound <- conn$createCompound(conn$downloadCompoundFileContent(id, save_as = paste0('test-uniprot-', id, '.xml')))

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
		
		# Check NCBI Gene ID
		if (hHasKey(compounds[[id]], 'geneid'))
			checkEquals(compound$getNcbiGeneId(), compounds[[id]][['geneid']])
		
		# Check Enzyme ID
		if (hHasKey(compounds[[id]], 'enzymeid'))
			checkEquals(compound$getEnzymeId(), compounds[[id]][['enzymeid']])

		# Check name
		checkTrue(compound$getName() != '')

		# Check fullname
		if (hHasKey(compounds[[id]], 'fullname'))
			checkEquals(compound$getFullName(), compounds[[id]][['fullname']])

		# Check gene symbol
		if (hHasKey(compounds[[id]], 'gene_symbol'))
			checkEquals(compound$getGeneSymbol(), compounds[[id]][['gene_symbol']])

		# Check length
		checkTrue(compound$getLength() > 0)

		# Check mass
		checkTrue(compound$getMass() > 0)

		# Check sequence
		if (hHasKey(compounds[[id]], 'seq'))
			checkEquals(compounds[[id]][['seq']], compound$getSequence())
	}
}
