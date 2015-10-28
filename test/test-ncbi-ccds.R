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

entries <- list('CCDS43240.1' = list(nucl_seq=paste('ATGAATCAAACTGCCATTCTGATTTGCTGCCTTATCTTTCTGACTCTAAGTGGCATTCAAGGAGTACCTC',
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

# Loop on all entries
for (id in names(entries)) {

	# Skip big entry (take too much time)
	if (hGetBool(entries[[id]], 'big') && ! full_test)
		next

	print(paste('Testing NCBI CCDS entry', id, '...'))

	# Get Entry from database
	entry <- conn$createEntry(conn$downloadEntryFileContent(id, save_as = paste0('test-ncbi-ccds-', id, '.html')))

	# This is a false entry => test that it's null
	if (hGetBool(entries[[id]], 'false'))
		checkTrue(is.null(entry))

	# This is a real entry => test that it isn't null
	else {
		checkTrue( ! is.null(entry))

		# Check that returned id is the same
		checkEquals(entry$getId(), id)

		# Check nucleotide sequence
		if (hHasKey(entries[[id]], 'nucl_seq'))
			checkEquals(entry$getNucleotideSequence(), entries[[id]][['nucl_seq']])
	}
}
