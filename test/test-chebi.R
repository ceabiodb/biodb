source(file.path(dirname(script.path), '..', 'ChebiConn.R'), chdir = TRUE)

#############
# CONSTANTS #
#############

ENTRIES <- list('2528' = list( inchi = 'InChI=1S/C27H44O7/c1-14(2)6-7-22(32)26(5,33)21-8-9-27(34)16-11-17(28)15-10-18(29)19(30)12-24(15,3)23(16)20(31)13-25(21,27)4/h11,14-15,18-23,29-34H,6-10,12-13H2,1-5H3/t15-,18+,19-,20+,21-,22+,23+,24-,25+,26+,27+/m0/s1', inchikey = 'LQGNCUXDDPRDJH-UKTRSHMFSA-N' ),
                'TOTO' = list(false = TRUE)
                )

#####################
# ONLINE TEST CHEBI #
#####################

online.test.chebi <- function() {

	# Open connexion
	conn <- ChebiConn$new(useragent = USER.AGENT)

	# Loop on all entries
	for (id in names(ENTRIES)) {

		print(paste('Testing ChEBI entry', id, '...'))

		# Get Entry from database
		entry <- conn$createEntry(conn$downloadEntryFileContent(id, save_as = paste0('test-chebi-', id, '.html')))

		# This is a false entry => test that it's null
		if (hGetBool(entries[[id]], 'false'))
			checkTrue(is.null(entry))

		# This is a real entry => test that it isn't null
		else {
			checkTrue( ! is.null(entry))

			# Check that returned id is the same
			checkEquals(entry$getId(), id)
			
			# Check inchi
			if (hHasKey(entries[[id]], 'inchi'))
				checkEquals(entry$getInchi(), entries[[id]][['inchi']])
			
			# Check inchi key
			if (hHasKey(entries[[id]], 'inchikey'))
				checkEquals(entry$getInchiKey(), entries[[id]][['inchikey']])
		}
	}
}
