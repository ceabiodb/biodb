# vi: fdm=marker

# Test ChemSpider wsFilterNamePost {{{1
################################################################

test.chemspider.wsFilterNamePost <- function(conn) {

	results <- conn$wsFilterNamePost('benzene')
	testthat::expect_true(nchar(results) > 0)

	ids <- conn$wsFilterNamePost('benzene', retfmt = 'ids')
	testthat::expect_is(ids, 'character')
	testthat::expect_true(length(ids) > 0)
}

# Test ChemSpider wsFilterMassPost {{{1
################################################################

test.chemspider.wsFilterMassPost <- function(conn) {

	results <- conn$wsFilterMassPost(mass = 96, range = 0.01, retfmt = 'ids')
	testthat::expect_is(results, 'character')
	testthat::expect_true(length(results) > 0)
}

# Test ChemSpider wsFilterQueryIdStatusGet {{{1
################################################################

test.chemspider.wsFilterQueryIdStatusGet <- function(conn) {

	results <- conn$wsFilterNamePost('benzene', retfmt = 'queryid')
	testthat::expect_is(results, 'character')
	testthat::expect_true( ! is.na(results))

	for(i in 1:10) {
		status <- conn$wsFilterQueryIdStatusGet(results, retfmt = 'status')
		testthat::expect_is(status, 'character')
		if (status == 'Complete')
			break
	}
	testthat::expect_equal(status, 'Complete')
}

# Test ChemSpider wsFilterQueryIdResultsGet {{{1
################################################################

test.chemspider.wsFilterQueryIdResultsGet <- function(conn) {

	results <- conn$wsFilterNamePost('benzene', retfmt = 'queryid')
	testthat::expect_is(results, 'character')
	testthat::expect_true( ! is.na(results))
	queryid <- results

	for(i in 1:10) {
		status <- conn$wsFilterQueryIdStatusGet(queryid, retfmt = 'status')
		testthat::expect_is(status, 'character')
		if (status == 'Complete')
			break
	}
	testthat::expect_equal(status, 'Complete')

	ids <- conn$wsFilterQueryIdResultsGet(queryid, retfmt = 'ids')
	testthat::expect_is(ids, 'integer')
	testthat::expect_length(ids, 1)
	testthat::expect_equal(ids, 236)
}

# Test ChemSpider wsRecordsRecordidDetailsGet {{{1
################################################################

test.chemspider.wsRecordsRecordidDetailsGet <- function(conn) {
	
	id <- 236
	results <- conn$wsRecordsRecordidDetailsGet(id, retfmt = 'parsed')
	testthat::expect_is(results, 'list')
	testthat::expect_equal(results$id, id)
}

# Test ChemSpider wsRecordsBatchPost {{{1
################################################################

test.chemspider.wsRecordsBatchPost <- function(conn) {
	
	ids <- c(236, 237)
	results <- conn$wsRecordsBatchPost(ids, retfmt = 'parsed')
	testthat::expect_is(results, 'list')
	testthat::expect_true('records' %in% names(results))
	testthat::expect_length(results$records, 2)
	testthat::expect_identical(sort(vapply(results$records, function(x) x$id, FUN.VALUE = 0)), sort(ids))
}

# Main {{{1
################################################################

biodb::testThat('ChemSpider web service filter-name-post works fine.', 'test.chemspider.wsFilterNamePost', conn = conn)
biodb::testThat('ChemSpider web service filter-mass-post works fine.', 'test.chemspider.wsFilterMassPost', conn = conn)
biodb::testThat('ChemSpider web service filter-queryId-status-get works fine.', 'test.chemspider.wsFilterQueryIdStatusGet', conn = conn)
biodb::testThat('ChemSpider web service filter-queryId-results-get works fine.', 'test.chemspider.wsFilterQueryIdResultsGet', conn = conn)
biodb::testThat('ChemSpider web service records-recordId-details-get works fine.', 'test.chemspider.wsRecordsRecordidDetailsGet', conn = conn)
biodb::testThat('ChemSpider web service records-batch-post works fine.', 'test.chemspider.wsRecordsBatchPost', conn = conn)
