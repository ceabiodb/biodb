# vi: fdm=marker

# Test enzyme-byname ExPASy Enzyme web service {{{1
################################################################

test.ws.enzymeByName <- function(db) {

	ids <- db$ws.enzymeByName("Alcohol", retfmt = 'ids')

	expect_gt(length(ids), 0)
	expect_equal(length(grep('^[0-9.]*$', ids)), length(ids))
}

# Test enzyme-bycomment ExPASy Enzyme web service {{{1
################################################################

test.ws.enzymeByComment <- function(db) {

	ids <- db$ws.enzymeByComment("best", retfmt = 'ids')

	expect_gt(length(ids), 0)
	expect_equal(length(grep('^[0-9.]*$', ids)), length(ids))
}

# Run tests {{{1
################################################################

run.expasy.enzyme.tests <- function(db, mode) {
	if (mode %in% c(MODE.ONLINE, MODE.QUICK.ONLINE)) {
		test.that('Calls to enzyme-byname ExPASy Enzyme web service work fine.', 'test.ws.enzymeByName', conn = db)
		test.that('Calls to enzyme-bycomment ExPASy Enzyme web service work fine.', 'test.ws.enzymeByComment', conn = db)
	}
}
