#!/usr/bin/env R --slave -f
library(RUnit)
source('../MirbaseConn.R', chdir = TRUE)
source('../../r-lib/hshhlp.R', chdir = TRUE)

options(error = function() { traceback(2) ; q(status = 1) }, warn = 2 )

compounds <- list('hsa-miR-142-3p'    = list(accession = 'MIMAT0000434',  sequence = 'UGUAGUGUUUCCUACUUUAUGGA'),
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

# Loop on all compounds
for (id in names(compounds)) {

	# Skip big compound (take too much time)
	if (hGetBool(compounds[[id]], 'big') && ! full_test)
		next

	print(paste('Testing MIRBASE compound', id, '...'))

	# Get Compound from database
	compound <- conn$createCompound(conn$downloadCompoundFileContent(id, save_as = paste0('test-mirbase-', id, '.html')))

	# This is a false compound => test that it's null
	if (hGetBool(compounds[[id]], 'false'))
		checkTrue(is.null(compound))

	# This is a real compound => test that it isn't null
	else {
		checkTrue( ! is.null(compound))

		# Check that returned id is the same
		checkEquals(id, compound$getId())
			
		# Check accession number
		if (hHasKey(compounds[[id]], 'accession'))
			checkEquals(compounds[[id]][['accession']], compound$getAccessionNumber())
			
		# Check sequence 
		if (hHasKey(compounds[[id]], 'sequence'))
			checkEquals(toupper(compounds[[id]][['sequence']]), toupper(compound$getSequence()))
	}
}
