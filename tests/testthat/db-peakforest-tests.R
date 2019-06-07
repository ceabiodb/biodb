# vi: fdm=marker

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

# Test RT match with different units {{{1
################################################################

test.peakforest.mass.rt.match.with.different.units <- function(db) {

	# Search for peaks using RT in seconds
	mz <- 118
	mz.tol <- 0.1
	rt <- 53
	rt.unit <- 's'
	rt.tol <- 30
	rt.tol.exp <- 0.8
	chrom.col.ids <- db$getChromCol()$id
	peaks.sec <- db$searchMsPeaks(mz = mz, mz.tol = mz.tol, rt = rt, rt.unit = rt.unit, rt.tol = rt.tol, rt.tol.exp = rt.tol.exp, chrom.col.ids = chrom.col.ids)
	expect_is(peaks.sec, 'data.frame')
	expect_gt(nrow(peaks.sec), 0)

	# Search for peaks using RT in minutes
	rt <- rt / 60
	rt.unit <- 'min'
	peaks.min <- db$searchMsPeaks(mz = mz, mz.tol = mz.tol, rt = rt, rt.unit = rt.unit, rt.tol = rt.tol, rt.tol.exp = rt.tol.exp, chrom.col.ids = chrom.col.ids)
	expect_is(peaks.min, 'data.frame')
	expect_gt(nrow(peaks.min), 0)

	# Test that results are identical
	expect_equal(nrow(peaks.sec), nrow(peaks.min))
	cols <- c('accession')
	expect_identical(peaks.sec[cols], peaks.min[cols])
}

# Test PeakForest Mass token init {{{1
################################################################

test.peakforest.mass.token.init <- function(biodb) {

	token <- 'ABCDEFGHIJKL'
	conn <- biodb$getFactory()$createConn('peakforest.mass', token = token, fail.if.exists = FALSE)
	testthat::expect_is(conn, 'PeakforestMassConn')
	testthat::expect_is(conn$getToken(), 'character')
	testthat::expect_length(conn$getToken(), 1)
	testthat::expect_equal(conn$getToken(), token)
	biodb$getFactory()$deleteConn(conn$getId())
}

# Run PeakForest Compound tests {{{1
################################################################

run.peakforest.compound.tests <- function(conn, obs) {
	if (test.online()) {
		test.that('Web service "search" works.', 'test.peakforest.compound.wsSearch', conn = conn)
		test.that('Web service "all.count" works.', 'test.peakforest.compound.wsAllCount', conn = conn)
		test.that('Web service "all.ids" works.', 'test.peakforest.compound.wsAllIds', conn = conn)
	}
}

# Run PeakForest Mass tests {{{1
################################################################

run.peakforest.mass.tests <- function(conn, obs) {

	test.that('Test token init.', 'test.peakforest.mass.token.init', biodb = conn$getBiodb())

	if (test.online()) {
		test.that('Test if RT match gives same result with minutes and seconds in input.', 'test.peakforest.mass.rt.match.with.different.units', conn = conn)
	}
}
