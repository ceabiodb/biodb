#!/usr/bin/env R --slave -f
library(RUnit)
source('../MirbaseConn.R', chdir = TRUE)
source('../../r-lib/hshhlp.R', chdir = TRUE)

entries <- list('hsa-miR-142-3p'    = list(accession = 'MIMAT0000434',  sequence = 'UGUAGUGUUUCCUACUUUAUGGA'),
                'hsa-miR-142-5p'    = list(accession = 'MIMAT0000433',  sequence = 'CAUAAAGUAGAAAGCACUACU'),
#                'hsa-miR-155'       = list(accession = 'MI0000681',     sequence = 'UUAAUGCUAAUCGUGAUAGGGG'),                                          
#                'hsa-miR-223'       = list(accession = '', sequence = 'UGUCAGUUUGUCAAAUACCCC'),                                           
#                'hsa-miR-146a'      = list(accession = '', sequence = 'UGAGAACUGAAUUCCAUGGGUU'),                                          
#                'hsa-miR-146b'      = list(accession = '', sequence = 'UGAGAACUGAAUUCCAUAGGCU'),                                          
#                'hsa-miR-342'       = list(accession = '', sequence = 'UCUCACACAGAAAUCGCACCCGUC'),                                            
#                'hsa-miR-30c'       = list(accession = '', sequence = 'UGUAAACAUCCUACACUCUCAGC'),                                            
#                'hsa-miR-30a-3p'    = list(accession = '', sequence = 'CUUUCAGUCGGAUGUUUGCAGC'),                                          
#                'hsa-miR-10a'       = list(accession = '', sequence = 'UACCCUGUAGAUCCGAAUUUGUG'),                                             
#                'hsa-miR-30e-3p'    = list(accession = '', sequence = 'CUUUCAGUCGGAUGUUUACAGC'),                                          
#                'hsa-miR-30b'       = list(accession = '', sequence = 'UGUAAACAUCCUACACUCAGCU'),                                          
#                'hsa-miR-125a'      = list(accession = '', sequence = 'UCCCUGAGACCCUUUAACCUGUG'),                                             
#                'hsa-miR-10b'       = list(accession = '', sequence = 'UACCCUGUAGAACCGAAUUUGU'),                                          
#                'hsa-miR-32'        = list(accession = '', sequence = 'UAUUGCACAUUACUAAGUUGC'),                                           
#                'hsa-let-7c'        = list(accession = '', sequence = 'UGAGGUAGUAGGUUGUAUGGUU'),                                          
#                'hsa-miR-200a'      = list(accession = '', sequence = 'UAACACUGUCUGGUAACGAUGU'),                                          
                'TAGADA' = list(false = TRUE)
                )

# Open connexion
conn <- MirbaseConn$new(useragent = "fr.cea.test-mirbase ; pierrick.rogermele@cea.fr")

# Loop on all entries
for (id in names(entries)) {

	# Skip big entry (take too much time)
	if (hGetBool(entries[[id]], 'big') && ! full_test)
		next

	print(paste('Testing MIRBASE entry', id, '...'))

	# Get Entry from database
	entry <- conn$createEntry(conn$downloadEntryFileContent(id, save_as = paste0('test-mirbase-', id, '.html')))

	# This is a false entry => test that it's null
	if (hGetBool(entries[[id]], 'false'))
		checkTrue(is.null(entry))

	# This is a real entry => test that it isn't null
	else {
		checkTrue( ! is.null(entry))

		# Check that returned id is the same
		checkEquals(id, entry$getId())
			
		# Check accession number
		if (hHasKey(entries[[id]], 'accession'))
			checkEquals(entries[[id]][['accession']], entry$getAccessionNumber())
			
		# Check sequence 
		if (hHasKey(entries[[id]], 'sequence'))
			checkEquals(toupper(entries[[id]][['sequence']]), toupper(entry$getSequence()))
	}
}
