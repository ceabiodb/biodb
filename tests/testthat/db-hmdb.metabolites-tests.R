# vi: fdm=marker

# Test HMDB Metabolite nb entries {{{1
################################################################

test.hmdbmetabolite.nbentries <- function(db) {

	# Check number of entries
	n <- db$getNbEntries(count = TRUE)
	expect_is(n, 'integer')
	if (db$isDownloaded())
		expect_gt(n, 0)
	else
		expect_true(is.na(n))
}

# Main {{{1
################################################################

biodb::testThat("HMDB metabolite returns enough entries ", test.hmdbmetabolite.nbentries, conn = conn)
