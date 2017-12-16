# vi: fdm=marker

# Test enzyme-search-DE ExPASy Enzyme web service {{{1
################################################################

test.ws.enzymeSearchDE <- function(db) {

	ids <- db$ws.enzymeSearchDE("Alcohol", biodb.ids = TRUE)

	expect_equal(length(grep('^[0-9.]*$', ids)), length(ids))
}

# Run tests {{{1
################################################################

run.expasy.enzyme.tests <- function(db, mode) {
	if (mode %in% c(MODE.ONLINE, MODE.QUICK.ONLINE)) {
		run.db.test('Test enzyme-search-DE ExPASy Enzyme web service.', 'test.ws.enzymeSearchDE', db)
	}
}

