# vi: fdm=marker

# Test HMDB Metabolite nb entries {{{1
################################################################

test.hmdbmetabolite.nbentries <- function(db) {

	# Check number of entries
	expect_gt(db$getNbEntries(count = TRUE), 4000)
}

# Run HMDB tests {{{1
################################################################

run.hmdb.metabolite.tests <- function(db, mode) {
	if (mode %in% c(MODE.ONLINE, MODE.QUICK.ONLINE))
		run.db.test("HMDB metabolite returns enough entries ", 'test.hmdbmetabolite.nbentries', db)
}
