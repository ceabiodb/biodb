#!/usr/bin/env Rscript
library(RUnit)
library(getopt)
source('../NcbiGeneConn.R', chdir = TRUE)
source('../NcbiCcdsConn.R', chdir = TRUE)
source('../../r-lib/hshhlp.R', chdir = TRUE)

####################
# GLOBAL CONSTANTS #
####################

ENTRIES <- list('9606' = list( keggid = NA_character_ ),
                '2139485387547754' = list(false = TRUE),
                '0' = list(false = TRUE),
                '-1' = list(false = TRUE),
                '7273' = list(big = TRUE),
                '3627' = list(symbol = 'CXCL10', fullname = 'chemokine (C-X-C motif) ligand 10',
							  synonyms = c('IFI10', 'C7', 'INP10', 'IP-10', 'crg-2', 'mob-1', 'SCYB10', 'gIP-10'),
							  location = '4q21',
							  keggid = 'hsa:3627',
							  ccds_id = 'CCDS43240.1',
							  sequence = 'ATGAATCAAACTGCCATTCTGATTTGCTGCCTTATCTTTCTGACTCTAAGTGGCATTCAAGGAGTACCTCTCTCTAGAACTGTACGCTGTACCTGCATCAGCATTAGTAATCAACCTGTTAATCCAAGGTCTTTAGAAAAACTTGAAATTATTCCTGCAAGCCAATTTTGTCCACGTGTTGAGATCATTGCTACAATGAAAAAGAAGGGTGAGAAGAGATGTCTGAATCCAGAATCGAAGGCCATCAAGAATTTACTGAAAGCAGTTAGCAAGGAAAGGTCTAAAAGATCTCCTTAA'),
                '2833' = list(symbol = 'CXCR3',
							  fullname = 'chemokine (C-X-C motif) receptor 3',
							  synonyms = c('GPR9', 'MigR', 'CD182', 'CD183', 'Mig-R', 'CKR-L2', 'CMKAR3', 'IP10-R')),
                '50943' = list(symbol = 'FOXP3',
							   fullname = 'forkhead box P3',
							   synonyms = c('JM2', 'AIID', 'IPEX', 'PIDX', 'XPID', 'DIETER')),
                '5551' = list(symbol = 'PRF1',
							  fullname = 'perforin 1 (pore forming protein)',
							  synonyms = c('P1', 'PFP', 'FLH2', 'PFN1', 'HPLH2')),
                '3002' = list(symbol = 'GZMB',
							  fullname = 'granzyme B (granzyme 2, cytotoxic T-lymphocyte-associated serine esterase 1)',
							  synonyms = c('HLP', 'CCPI', 'CGL1', 'CSPB', 'SECT', 'CGL-1', 'CSP-B', 'CTLA1', 'CTSGL1')),
                '916' = list(symbol = 'CD3E',
							 fullname = 'CD3e molecule, epsilon (CD3-TCR complex)',
							 synonyms = c('T3E', 'TCRE', 'IMD18'))
                )

#############
# READ ARGS #
#############

read_args <- function() {
  
  # program name
  prog <- sub('^.*/([^/]+)$', '\\1', commandArgs()[4], perl = TRUE)
  
  # options
  spec = matrix(c(
    'full',         'f', 0, 'logical',      'Full test. Disabled by default.',
    'help',         'h', 0, 'logical',      'Print this help.',
    'seq',          's', 0, 'logical',      'Sequence test (test retrieving biological sequences). Disabled by default.'
  ), byrow = TRUE, ncol = 5)
   
  opt <- getopt(spec)
  opt$full = if (is.null(opt$full)) FALSE else TRUE
  opt$seq = if (is.null(opt$seq)) opt$full else TRUE

  # help
  if ( ! is.null(opt$help)) {
    cat(getopt(spec, usage = TRUE, command = prog))
    q(status = 1)
  }

  return(opt)
}

################
# TEST ENTRIES #
################

test_entries <- function(entries, user_agent, full_test = FALSE, seq_test = FALSE) {

	conn <- NcbiGeneConn$new(useragent = user_agent)
	ccds_conn <- NULL

	# Loop on all entries
	for (id in names(entries)) {

		# Skip big entry (take too much time)
		if ( ! full_test && hGetBool(entries[[id]], 'big'))
			next

		print(paste('Testing NCBI entry', id, '...'))

		# Get Entry from database
		entry <- conn$createEntry(conn$downloadEntryFileContent(id, save_as = paste0('test-ncbi-gene-', id, '.xml')))

		# This is a false entry => test that it's null
		if (hGetBool(entries[[id]], 'false'))
			checkTrue(is.null(entry))

		# This is a real entry => test that it isn't null
		else {
			checkTrue( ! is.null(entry))

			# Check that returned id is the same
			checkEquals(entry$getId(), id)
			
			# Check symbol
			if (hHasKey(entries[[id]], 'symbol'))
				checkEquals(entry$getSymbol(), entries[[id]][['symbol']])
			
			# Check full name
			if (hHasKey(entries[[id]], 'fullname'))
				checkEquals(entry$getOfficialFullName(), entries[[id]][['fullname']])
			
			# Check location
			if (hHasKey(entries[[id]], 'location'))
				checkEquals(entry$getLocation(), entries[[id]][['location']])
			
			# Check Kegg ID
			if (hHasKey(entries[[id]], 'keggid'))
				checkEquals(entry$getKeggId(), entries[[id]][['keggid']])

			# Check synonyms
			if (hHasKey(entries[[id]], 'synonyms'))
				checkEquals(sort(entry$getSynonyms()), sort(entries[[id]][['synonyms']]))

			# Check CCDS ID
			if (hHasKey(entries[[id]], 'ccds_id')) {
				checkEquals(sort(entry$getCcdsId()), sort(entries[[id]][['ccds_id']]))

				# Check sequence
				if (seq_test && hHasKey(entries[[id]], 'sequence')) {
					if (is.null(ccds_conn))
						ccds_conn <- NcbiCcdsConn$new(useragent = user_agent)
					checkEquals(ccds_conn$getEntry(entry$getCcdsId())$getNucleotideSequence(), entries[[id]][['sequence']])
				}
			}
		}
	}
}

########
# MAIN #
########
opt<-read_args()
test_entries(entries = ENTRIES, user_agent = "fr.cea.r-biodb.test-ncbi-gene ; pierrick.rogermele@cea.fr", full_test = opt$full, seq_test = opt$seq)
