#############
# CONSTANTS #
#############

#MASSBANK.ENTRIES <- read.table('spectra-massbank.txt', stringsAsFactors = FALSE, header = TRUE)

########################
# ONLINE TEST MASSBANK #
########################

offline.test.massbank <- function() {
	test.entries(RBIODB.MASSBANK, RBIODB.SPECTRUM, online = FALSE)
}

online.test.massbank <- function() {
	test.entries(RBIODB.MASSBANK, RBIODB.SPECTRUM, online = TRUE)
}
