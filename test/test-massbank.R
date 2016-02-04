#############
# CONSTANTS #
#############

MASSBANK.ENTRIES <- list(
	list(type = RBIODB.MASSBANK, id = 'KOX00001', inchi = 'InChI=1S/C4H9NO2/c5-3-1-2-4(6)7/h1-3,5H2,(H,6,7)', name = 'GABA', chebiid = '30566', pubchemid = 'SID:3628', keggid = 'C00334'),
	list(type = RBIODB.MASSBANK, id = 'TOTO', false = TRUE)
                )
# TODO make a list of list: list(list(id = ..., inchi = ...), list(id = ..., false = TRUE)) OR a date.frame (read from a file ?)

########################
# ONLINE TEST MASSBANK #
########################

online.test.massbank <- function() {
	online.test(MASSBANK.ENTRIES)
}

