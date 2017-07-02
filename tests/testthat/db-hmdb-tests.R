# vi: fdm=marker

# Test HMDB Metabolite nb entries {{{1
################################################################

test.hmdbmetabolite.nbentries <- function(db) {

	# Check number of entries
	expect_gt(db$getNbEntries(count = TRUE), 4000)
}
