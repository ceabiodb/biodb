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
	rt.tol <- rt.tol / 60
	peaks.min <- db$searchMsPeaks(mz = mz, mz.tol = mz.tol, rt = rt, rt.unit = rt.unit, rt.tol = rt.tol, rt.tol.exp = rt.tol.exp, chrom.col.ids = chrom.col.ids)
	expect_is(peaks.min, 'data.frame')
	expect_gt(nrow(peaks.min), 0)

	# Test that results are identical
	expect_equal(nrow(peaks.sec), nrow(peaks.min))
	cols <- c('accession')
	expect_identical(peaks.sec[cols], peaks.min[cols])
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

# Run PeakForest Mass tests {{{1
################################################################

run.peakforest.mass.tests <- function(db, mode) {
	if (mode %in% c(MODE.ONLINE, MODE.QUICK.ONLINE)) {
		run.db.test.that('Test if RT match gives same result with minutes and seconds in input.', 'test.peakforest.mass.rt.match.with.different.units', db)
	}
}
