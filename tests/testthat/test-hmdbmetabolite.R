# vi: fdm=marker

# Test HMDB Metabolite nb entries {{{1
################################################################

test.hmdbmetabolite.nbentries <- function(db) {

	# Check number of entries
	expect_gt(db$getNbEntries(count = TRUE), 4000)
}

# MAIN {{{1
################################################################

if (length(TEST.DATABASES) == 0 || BIODB.HMDBMETABOLITE %in% TEST.DATABASES) {

	context("Testing massfiledb offline")

	# Create biodb instance
	biodb <- Biodb$new(logger = FALSE)
	biodb$addObservers(BiodbLogger$new(file = LOG.FILE))
	factory <- biodb$getFactory()

	# Create database
	db <- factory$createConn(BIODB.HMDBMETABOLITE)

	if (ONLINE %in% TEST.MODES)
		test_that("HMDB metabolite returns enough entries ", test.hmdbmetabolite.nbentries(db))
}
