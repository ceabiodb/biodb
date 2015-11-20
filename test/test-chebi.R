source(file.path(dirname(script.path), '..', 'ChebiConn.R'), chdir = TRUE)

#############
# CONSTANTS #
#############

ENTRIES <- list(list(id = '2528', inchi = 'InChI=1S/C27H44O7/c1-14(2)6-7-22(32)26(5,33)21-8-9-27(34)16-11-17(28)15-10-18(29)19(30)12-24(15,3)23(16)20(31)13-25(21,27)4/h11,14-15,18-23,29-34H,6-10,12-13H2,1-5H3/t15-,18+,19-,20+,21-,22+,23+,24-,25+,26+,27+/m0/s1', inchikey = 'LQGNCUXDDPRDJH-UKTRSHMFSA-N' ),
                list(id = 'TOTO', false = TRUE)
                )
# TODO make a list of list: list(list(id = ..., inchi = ...), list(id = ..., false = TRUE)) OR a date.frame (read from a file ?)

#####################
# ONLINE TEST CHEBI #
#####################

online.test.chebi <- function() {

	# Open connexion
	conn <- ChebiConn$new(useragent = USER.AGENT)

	# Loop on all entries
	for (e in ENTRIES) {

		# Get ID
		if ( ! 'id' %in% names(e))
			stop('One element of ENTRIES does not contain an "id" field.')
		id <- e[['id']]

		# Display information message TODO maybe display it only if verbose or debug flag is on
		print(paste('Testing ChEBI entry', id, '...'))

		# Get Entry from database
		entry <- conn$createEntry(conn$downloadEntryFileContent(id, save_as = paste0('test-chebi-', id, '.html')))

		for (f in names(e))
			switch(f,
			       id = if (! 'false' %in% names(e)) checkEquals(entry$getId(), id),
			       false = checkTrue(is.null(entry)),
			       inchi = checkEquals(entry$getInchi(), e[['inchi']]),
			       inchikey = checkEquals(entry$getInchiKey(), e[['inchikey']])
			      )

	}
}
