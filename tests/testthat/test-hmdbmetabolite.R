# vi: fdm=marker

# Test HMDB Metabolite nb entries {{{1
################################################################

test.hmdbmetabolite.nbentries <- function(db) {

	# Check number of entries
	expect_gt(db$getNbEntries(count = TRUE), 4000)
}

# MAIN {{{1
################################################################

if (length(TEST.DATABASES) == 0 || BIODB.HMDB.METABOLITE %in% TEST.DATABASES) {

	context("Testing hmdbmetabolite")

	# Create biodb instance
	biodb <- Biodb$new(logger = FALSE, observers = BiodbLogger$new(file = LOG.FILE))
	biodb$getConfig()$set(CFG.CACHE.DIRECTORY, CACHE.DIR)
	factory <- biodb$getFactory()

	# Create database
	db <- factory$createConn(BIODB.HMDB.METABOLITE)

	if (MODE.ONLINE %in% TEST.MODES)
		test_that("HMDB metabolite returns enough entries ", test.hmdbmetabolite.nbentries(db))
}
