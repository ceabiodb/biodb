# vi: fdm=marker

# Test ChemSpider ws.filterNamePost {{{1
################################################################

test.chemspider.ws.filterNamePost <- function(conn) {

	results <- conn$ws.filterNamePost('benzene')
	testthat::expect_true(nchar(results) > 0)

	ids <- conn$ws.filterNamePost('benzene', retfmt = 'ids')
	testthat::expect_is(ids, 'character')
	testthat::expect_true(length(ids) > 0)
}

# Test ChemSpider ws.filterMassPost {{{1
################################################################

test.chemspider.ws.filterMassPost <- function(conn) {

	results <- conn$ws.filterMassPost(mass = 96, range = 0.01, retfmt = 'ids')
	testthat::expect_is(results, 'character')
	testthat::expect_true(length(results) > 0)
}

# Test ChemSpider ws.filterQueryIdStatusGet {{{1
################################################################

test.chemspider.ws.filterQueryIdStatusGet <- function(conn) {

	results <- conn$ws.filterNamePost('benzene', retfmt = 'queryid')
	testthat::expect_is(results, 'character')
	testthat::expect_true( ! is.na(results))

	for(i in 1:10) {
		status <- conn$ws.filterQueryIdStatusGet(results, retfmt = 'status')
		testthat::expect_is(status, 'character')
		if (status == 'Complete')
			break
	}
	testthat::expect_equal(status, 'Complete')
}

# Test ChemSpider ws.filterQueryIdResultsGet {{{1
################################################################

test.chemspider.ws.filterQueryIdResultsGet <- function(conn) {

	results <- conn$ws.filterNamePost('benzene', retfmt = 'queryid')
	testthat::expect_is(results, 'character')
	testthat::expect_true( ! is.na(results))
	queryid <- results

	for(i in 1:10) {
		status <- conn$ws.filterQueryIdStatusGet(queryid, retfmt = 'status')
		testthat::expect_is(status, 'character')
		if (status == 'Complete')
			break
	}
	testthat::expect_equal(status, 'Complete')

	ids <- conn$ws.filterQueryIdResultsGet(queryid, retfmt = 'ids')
	testthat::expect_is(ids, 'integer')
	testthat::expect_length(ids, 1)
	testthat::expect_equal(ids, 236)
}

# Test ChemSpider ws.recordsRecordidDetailsGet {{{1
################################################################

test.chemspider.ws.recordsRecordidDetailsGet <- function(conn) {
	
	id <- 236
	results <- conn$ws.recordsRecordidDetailsGet(id, retfmt = 'parsed')
	testthat::expect_is(results, 'list')
	testthat::expect_equal(results$id, id)
}

# Test ChemSpider ws.recordsBatchPost {{{1
################################################################

test.chemspider.ws.recordsBatchPost <- function(conn) {
	
	ids <- c(236, 237)
	results <- conn$ws.recordsBatchPost(ids, retfmt = 'parsed')
	testthat::expect_is(results, 'list')
	testthat::expect_true('records' %in% names(results))
	testthat::expect_length(results$records, 2)
	testthat::expect_identical(sort(vapply(results$records, function(x) x$id, FUN.VALUE = 0)), sort(ids))
}

# Run ChemSpider tests {{{1
################################################################

run.chemspider.tests <- function(conn, obs) {
	if (test.online()) {
		test.that('ChemSpider web service filter-name-post works fine.', 'test.chemspider.ws.filterNamePost', conn = conn)
		test.that('ChemSpider web service filter-mass-post works fine.', 'test.chemspider.ws.filterMassPost', conn = conn)
		test.that('ChemSpider web service filter-queryId-status-get works fine.', 'test.chemspider.ws.filterQueryIdStatusGet', conn = conn)
		test.that('ChemSpider web service filter-queryId-results-get works fine.', 'test.chemspider.ws.filterQueryIdResultsGet', conn = conn)
		test.that('ChemSpider web service records-recordId-details-get works fine.', 'test.chemspider.ws.recordsRecordidDetailsGet', conn = conn)
		test.that('ChemSpider web service records-batch-post works fine.', 'test.chemspider.ws.recordsBatchPost', conn = conn)
	}
}
