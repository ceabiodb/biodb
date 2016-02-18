#############
# CONSTANTS #
#############

#MASSBANK.ENTRIES <- data.frame( accession = c('KOX00001', 'EA256108', 'TOTO'), false = c(FALSE,FALSE,TRUE), msdev = c('Qstar, Applied Biosystems', '', NA_character_), stringsAsFactors = FALSE)
MASSBANK.ENTRIES <- read.table('spectra-massbank.txt', stringsAsFactors = FALSE, header = TRUE)
#	list(db = RBIODB.MASSBANK, type = RBIOD.SPECTRUM, id = 
#	list(db = RBIODB.MASSBANK, type = RBIOD.SPECTRUM, id = ),
#	list(db = RBIODB.MASSBANK, type = RBIOD.SPECTRUM, id = , false = TRUE)
#                )
# TODO make a list of list: list(list(id = ..., inchi = ...), list(id = ..., false = TRUE)) OR a date.frame (read from a file ?)

########################
# ONLINE TEST MASSBANK #
########################

online.test.massbank <- function() {
	online.test(RBIODB.MASSBANK, RBIODB.SPECTRUM, MASSBANK.ENTRIES)
}
