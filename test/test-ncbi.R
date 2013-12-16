#!/usr/bin/env R --slave -f
library(RUnit)
source('../NcbiConn.R', chdir = TRUE)
source('hash-helpers.R', chdir = TRUE)

full_test <- FALSE
# TODO add a flag for running long tests
#args <- commandArgs(trailingOnly = TRUE)
#full_test = args[1]

entries <- list('9606' = list(),
                '2139485387547754' = list(false = TRUE),
                '7273' = list(big = TRUE),
                '3627' = list(symbol = 'CXCL10', fullname = 'chemokine (C-X-C motif) ligand 10', synonymes = c('C7', 'IFI10', 'INP10', 'IP-10', 'crg-2', 'mob-1', 'SCYB10', 'gIP-10', location = '4q21')),
                '2833' = list(symbol = 'CXCR3', fullname = 'chemokine (C-X-C motif) receptor 3', synonymes = c('GPR9', 'MigR', 'CD182', 'CD183', 'Mig-R', 'CKR-L2', 'CMKAR3', 'IP10-R')),
                '50943' = list(symbol = 'FOXP3', fullname = 'forkhead box P3', synonymes = c('JM2', 'AIID', 'IPEX', 'PIDX', 'XPID', 'DIETER')),
                '5551' = list(symbol = 'PRF1', fullname = 'perforin 1 (pore forming protein)', synonymes = c('P1', 'PFP', 'FLH2', 'PFN1', 'HPLH2')),
                '3002' = list(symbol = 'GZMB', fullname = 'granzyme B (granzyme 2, cytotoxic T-lymphocyte-associated serine esterase 1)', synonymes = c('HLP', 'CCPI', 'CGL1', 'CSPB', 'SECT', 'CGL-1', 'CSP-B', 'CTLA1', 'CTSGL1')),
                '916' = list(symbol = 'CD3E', fullname = 'CD3e molecule, epsilon (CD3-TCR complex)', synonymes = c('T3E', 'TCRE'))
                )

# Open connexion
conn <- NcbiConn$new(useragent = "fr.cea.test-ncbi ; pierrick.rogermele@cea.fr")

# Loop on all entries
for (id in names(entries)) {

	# Skip big entry (take too much time)
	if (hGetBool(entries[[id]], 'big') && ! full_test)
		next

	print(paste('Testing NCBI entry', id, '...'))

	# Get Entry from database
	entry <- conn$getGeneEntry(as.numeric(id))

	# This is a false entry => test that it's null
	if (hGetBool(entries[[id]], 'false'))
		checkTrue(is.null(entry))

	# This is a real entry => test that it isn't null
	else {
		checkTrue( ! is.null(entry))

		# Check that returned id is the same
		checkEquals(entry$getId(), as.numeric(id))
		
		# Check symbol
		if (hHasKey(entries[[id]], 'symbol'))
			checkEquals(entry$getSymbol(), entries[[id]][['symbol']])
		
		# Check full name
		if (hHasKey(entries[[id]], 'fullname'))
			checkEquals(entry$getOfficialFullName(), entries[[id]][['fullname']])
		
		# Check location
		if (hHasKey(entries[[id]], 'location'))
			checkEquals(entry$getLocation(), entries[[id]][['location']])

		# save
		entry$save(paste('test-ncbi-', id, '.xml', sep=''))
	}
}
