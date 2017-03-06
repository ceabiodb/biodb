# vi: fdm=marker

source('common.R')

# Test HMDB Metabolite nb entries {{{1
################################################################

test.hmdbmetabolite.nbentries <- function(db) {

	# Check number of entries
	expect_gt(db$getNbEntries(count = TRUE), 4000)
}

# MAIN {{{1
################################################################

if (BIODB.HMDB.METABOLITE %in% TEST.DATABASES) {

	if (MODE.ONLINE %in% TEST.MODES) {

		# Create biodb instance
		biodb <- create.biodb.instance()
		set.test.context(biodb, "Testing hmdbmetabolite")
		set.mode(biodb, MODE.ONLINE)
		factory <- biodb$getFactory()

		# Create database
		db <- factory$createConn(BIODB.HMDB.METABOLITE)

		test_that("HMDB metabolite returns enough entries ", test.hmdbmetabolite.nbentries(db))
	}
}
