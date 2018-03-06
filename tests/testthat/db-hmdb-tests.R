# vi: fdm=marker

# Test HMDB Metabolite nb entries {{{1
################################################################

test.hmdbmetabolite.nbentries <- function(db) {

	# Check number of entries
	n <- db$getNbEntries(count = TRUE)
	expect_is(n, 'integer')
	if (db$isDownloaded())
		expect_gt(n, 4000)
	else
		expect_true(is.na(n))
}

# Run HMDB tests {{{1
################################################################

run.hmdb.metabolites.tests <- function(db, mode) {
	if (mode %in% c(MODE.ONLINE, MODE.QUICK.ONLINE))
		run.db.test.that("HMDB metabolite returns enough entries ", 'test.hmdbmetabolite.nbentries', db)
}
