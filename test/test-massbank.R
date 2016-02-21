#############
# CONSTANTS #
#############

#MASSBANK.ENTRIES <- read.table('spectra-massbank.txt', stringsAsFactors = FALSE, header = TRUE)

########################
# ONLINE TEST MASSBANK #
########################

online.test.massbank <- function() {
	test.entries(RBIODB.MASSBANK, RBIODB.SPECTRUM, online = FALSE)
}
