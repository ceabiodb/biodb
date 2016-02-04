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
                '4316' = list(symbol = 'MMP7', uniprotid = 'P09237'),
                '3627' = list(symbol = 'CXCL10', fullname = 'chemokine (C-X-C motif) ligand 10',
							  synonyms = c('IFI10', 'C7', 'INP10', 'IP-10', 'crg-2', 'mob-1', 'SCYB10', 'gIP-10'),
							  location = '4q21',
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

test_compounds <- function(compounds, user_agent, full_test = FALSE, seq_test = FALSE) {

	conn <- NcbiGeneConn$new(useragent = user_agent)
	ccds_conn <- NULL

	# Loop on all compounds
	for (id in names(compounds)) {

		# Skip big compound (take too much time)
		if ( ! full_test && hGetBool(compounds[[id]], 'big'))
			next

		print(paste('Testing NCBI compound', id, '...'))

		# Get Compound from database
		compound <- conn$createCompound(conn$downloadCompoundFileContent(id, save_as = paste0('test-ncbi-gene-', id, '.xml')))

		# This is a false compound => test that it's null
		if (hGetBool(compounds[[id]], 'false'))
			checkTrue(is.null(compound))

		# This is a real compound => test that it isn't null
		else {
			checkTrue( ! is.null(compound))

			# Check that returned id is the same
			checkEquals(compound$getId(), id)
			
			# Check symbol
			if (hHasKey(compounds[[id]], 'symbol'))
				checkEquals(compound$getSymbol(), compounds[[id]][['symbol']])
			
			# Check full name
			if (hHasKey(compounds[[id]], 'fullname'))
				checkEquals(compound$getOfficialFullName(), compounds[[id]][['fullname']])
			
			# Check location
			if (hHasKey(compounds[[id]], 'location'))
				checkEquals(compound$getLocation(), compounds[[id]][['location']])
			
			# Check Kegg ID
			if (hHasKey(compounds[[id]], 'keggid'))
				checkEquals(compound$getKeggId(), compounds[[id]][['keggid']])
			
			# Check UniProtKB/Swiss-Prot ID
			if (hHasKey(compounds[[id]], 'uniprotid'))
				checkEquals(compound$getUniProtId(), compounds[[id]][['uniprotid']])

			# Check synonyms
			if (hHasKey(compounds[[id]], 'synonyms'))
				checkEquals(sort(compound$getSynonyms()), sort(compounds[[id]][['synonyms']]))

			# Check CCDS ID
			if (hHasKey(compounds[[id]], 'ccds_id')) {
				checkEquals(sort(compound$getCcdsId()), sort(compounds[[id]][['ccds_id']]))

				# Check sequence
				if (seq_test && hHasKey(compounds[[id]], 'sequence')) {
					if (is.null(ccds_conn))
						ccds_conn <- NcbiCcdsConn$new(useragent = user_agent)
					checkEquals(ccds_conn$getCompound(compound$getCcdsId())$getNucleotideSequence(), compounds[[id]][['sequence']])
				}
			}
		}
	}
}

########
# MAIN #
########

options(error = function() { traceback(2) ; q(status = 1) }, warn = 2 )

opt<-read_args()
test_compounds(compounds = ENTRIES, user_agent = "fr.cea.r-biodb.test-ncbi-gene ; pierrick.rogermele@cea.fr", full_test = opt$full, seq_test = opt$seq)
