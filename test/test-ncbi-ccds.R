#!/usr/bin/env R --slave -f
library(RUnit)
source('../NcbiCcdsConn.R', chdir = TRUE)
source('../../r-lib/hshhlp.R', chdir = TRUE)

########
# MAIN #
########

options(error = function() { traceback(2) ; q(status = 1) }, warn = 2 )

full_test <- FALSE
# TODO add a flag for running long tests
#args <- commandArgs(trailingOnly = TRUE)
#full_test = args[1]

compounds <- list('CCDS43240.1' = list(nucl_seq=paste('ATGAATCAAACTGCCATTCTGATTTGCTGCCTTATCTTTCTGACTCTAAGTGGCATTCAAGGAGTACCTC',
													'TCTCTAGAACTGTACGCTGTACCTGCATCAGCATTAGTAATCAACCTGTTAATCCAAGGTCTTTAGAAAA',
													'ACTTGAAATTATTCCTGCAAGCCAATTTTGTCCACGTGTTGAGATCATTGCTACAATGAAAAAGAAGGGT',
													'GAGAAGAGATGTCTGAATCCAGAATCGAAGGCCATCAAGAATTTACTGAAAGCAGTTAGCAAGGAAAGGT',
													'CTAAAAGATCTCCTTAA',
 												   	sep='')),
                'CCDS12227.1' = list(nucl_seq=paste('ATGCCTACTGGAGACTTTGATTCGAAGCCCAGTTGGGCCGACCAGGTGGAGGAGGAGGGGGAGGACGACA',
												'AATGTGTCACCAGCGAGCTCCTCAAGGGGATCCCTCTGGCCACAGGTGACACCAGCCCAGAGCCAGAGCT',
												'ACTGCCGGGAGCTCCACTGCCGCCTCCCAAGGAGGTCATCAACGGAAACATAAAGACAGTGACAGAGTAC',
												'AAGATAGATGAGGATGGCAAGAAGTTCAAGATTGTCCGCACCTTCAGGATTGAGACCCGGAAGGCTTCAA',
												'AGGCTGTCGCAAGGAGGAAGAACTGGAAGAAGTTCGGGAACTCAGAGTTTGACCCCCCCGGACCCAATGT',
												'GGCCACCACCACTGTCAGTGACGATGTCTCTATGACGTTCATCACCAGCAAAGAGGACCTGAACTGCCAG',
												'GAGGAGGAGGACCCTATGAACAAACTCAAGGGCCAGAAGATCGTGTCCTGCCGCATCTGCAAGGGCGACC',
												'ACTGGACCACCCGCTGCCCCTACAAGGATACGCTGGGGCCCATGCAGAAGGAGCTGGCCGAGCAGCTGGG',
												'CCTGTCTACTGGCGAGAAGGAGAAGCTGCCGGGAGAGCTAGAGCCGGTGCAGGCCACGCAGAACAAGACA',
												'GGGAAGTATGTGCCGCCGAGCCTGCGCGACGGGGCCAGCCGCCGCGGGGAGTCCATGCAGCCCAACCGCA',
												'GAGCCGACGACAACGCCACCATCCGTGTCACCAACTTGTCAGAGGACACGCGTGAGACCGACCTGCAGGA',
												'GCTCTTCCGGCCTTTCGGCTCCATCTCCCGCATCTACCTGGCTAAGGACAAGACCACTGGCCAATCCAAG',
												'GGCTTTGCCTTCATCAGCTTCCACCGCCGCGAGGATGCTGCGCGTGCCATTGCCGGGGTGTCCGGCTTTG',
												'GCTACGACCACCTCATCCTCAACGTCGAGTGGGCCAAGCCGTCCACCAACTAA',
												sep='')),
                'TOTO' = list(false = TRUE)
                )

# Open connexion
conn <- NcbiCcdsConn$new(useragent = "fr.cea.test-ncbi-ccds ; pierrick.rogermele@cea.fr")

# Loop on all compounds
for (id in names(compounds)) {

	# Skip big compound (take too much time)
	if (hGetBool(compounds[[id]], 'big') && ! full_test)
		next

	print(paste('Testing NCBI CCDS compound', id, '...'))

	# Get Compound from database
	compound <- conn$createCompound(conn$downloadCompoundFileContent(id, save_as = paste0('test-ncbi-ccds-', id, '.html')))

	# This is a false compound => test that it's null
	if (hGetBool(compounds[[id]], 'false'))
		checkTrue(is.null(compound))

	# This is a real compound => test that it isn't null
	else {
		checkTrue( ! is.null(compound))

		# Check that returned id is the same
		checkEquals(compound$getId(), id)

		# Check nucleotide sequence
		if (hHasKey(compounds[[id]], 'nucl_seq'))
			checkEquals(compound$getNucleotideSequence(), compounds[[id]][['nucl_seq']])
	}
}
