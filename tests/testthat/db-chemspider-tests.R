# vi: fdm=marker

# Test ChemSpider ws.filterNamePost {{{1
################################################################

test.chemspider.ws.filterNamePost <- function(conn) {

	results <- conn$ws.filterNamePost('benzene')
	expect_true(nchar(results) > 0)

	ids <- conn$ws.filterNamePost('benzene', retfmt = 'ids')
	expect_is(ids, 'character')
	expect_true(length(ids) > 0)
}

# Test ChemSpider ws.filterMassPost {{{1
################################################################

test.chemspider.ws.filterMassPost <- function(conn) {

	results <- conn$ws.filterMassPost(mass = 96, range = 0.01, retfmt = 'ids')
	testthat::expect_is(results, 'character')
	testthat::expect_true(length(results) > 0)
}

# Run ChemSpider tests {{{1
################################################################

run.chemspider.tests <- function(conn, mode) {
	if (mode %in% c(MODE.ONLINE, MODE.QUICK.ONLINE)) {
		test.that('ChemSpider web service filter-name-post works fine.', 'test.chemspider.ws.filterNamePost', conn = conn)
		test.that('ChemSpider web service filter-mass-post works fine.', 'test.chemspider.ws.filterMassPost', conn = conn)
	}
}
