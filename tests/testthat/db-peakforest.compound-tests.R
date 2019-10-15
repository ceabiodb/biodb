# vi: fdm=marker ts=4 et cc=80 tw=80

# Test web service "search" {{{1
################################################################

test.peakforest.compound.wsSearch <- function(db) {

	# Plain search by name
	results <- db$wsSearch('acid', max = 2)
	expect_is(results, 'character')
	expect_true( ! is.na(results))
	expect_length(results, 1)
	expect_true(nchar(results) > 0)

	# Get parsed JSON
	results <- db$wsSearch('acid', max = 2, retfmt = 'parsed')
	expect_is(results, 'list')
	expect_length(results, 2)

	# Get parsed JSON
	results <- db$wsSearch('acid', max = 2, retfmt = 'ids')
	expect_is(results, 'character')
	expect_length(results, 2)
}

# Test web service "all.count" {{{1
################################################################

test.peakforest.compound.wsAllCount <- function(db) {
	results <- db$wsAllCount()
	expect_is(results, 'character')
	expect_length(results, 1)
	results <- db$wsAllCount(retfmt = 'parsed')
	expect_is(results, 'integer')
	expect_length(results, 1)
	expect_gt(results, 0)
}

# Test web service "all.ids" {{{1
################################################################

test.peakforest.compound.wsAllIds <- function(db) {
	results <- db$wsAllIds()
	expect_is(results, 'character')
	expect_length(results, 1)
	results <- db$wsAllIds(retfmt = 'parsed')
	expect_is(results, 'integer')
	expect_true(length(results) > 0)
	results <- db$wsAllIds(retfmt = 'ids')
	expect_is(results, 'character')
	expect_true(length(results) > 0)
}

# Main {{{1
################################################################

biodb::testThat('Web service "search" works.', test.peakforest.compound.wsSearch, conn = conn)
biodb::testThat('Web service "all.count" works.', test.peakforest.compound.wsAllCount, conn = conn)
biodb::testThat('Web service "all.ids" works.', test.peakforest.compound.wsAllIds, conn = conn)
