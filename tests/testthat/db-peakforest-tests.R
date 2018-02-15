# vi: fdm=marker

# Test web service "search" {{{1
################################################################

test.peakforest.compound.ws.search <- function(db) {

	# Plain search by name
	results <- db$ws.search('acid', max = 2)
	expect_is(results, 'character')
	expect_true( ! is.na(results))
	expect_length(results, 1)
	expect_true(nchar(results) > 0)

	# Get parsed JSON
	results <- db$ws.search('acid', max = 2, biodb.parse = TRUE)
	expect_is(results, 'list')
	expect_length(results, 2)

	# Get parsed JSON
	results <- db$ws.search('acid', max = 2, biodb.ids = TRUE)
	expect_is(results, 'character')
	expect_length(results, 2)
}

# Test web service "all.count" {{{1
################################################################

test.peakforest.compound.ws.all.count <- function(db) {
	results <- db$ws.all.count()
	expect_is(results, 'character')
	expect_length(results, 1)
	results <- db$ws.all.count(biodb.parse = TRUE)
	expect_is(results, 'integer')
	expect_length(results, 1)
	expect_gt(results, 0)
}

# Test web service "all.ids" {{{1
################################################################

test.peakforest.compound.ws.all.ids <- function(db) {
	results <- db$ws.all.ids()
	expect_is(results, 'character')
	expect_length(results, 1)
	results <- db$ws.all.ids(biodb.parse = TRUE)
	expect_is(results, 'integer')
	expect_true(length(results) > 0)
	results <- db$ws.all.ids(biodb.ids = TRUE)
	expect_is(results, 'character')
	expect_true(length(results) > 0)
}

# Run PeakForest Compound tests {{{1
################################################################

run.peakforest.compound.tests <- function(db, mode) {
	if (mode %in% c(MODE.ONLINE, MODE.QUICK.ONLINE)) {
		run.db.test.that('Web service "search" works.', 'test.peakforest.compound.ws.search', db)
		run.db.test.that('Web service "all.count" works.', 'test.peakforest.compound.ws.all.count', db)
		run.db.test.that('Web service "all.ids" works.', 'test.peakforest.compound.ws.all.ids', db)
	}
}
