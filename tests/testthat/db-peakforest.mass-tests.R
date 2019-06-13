# vi: fdm=marker ts=4 et cc=80

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

# Main {{{1
################################################################

test.that('Test token init.', 'test.peakforest.mass.token.init', biodb = conn$getBiodb())

test.that('Test if RT match gives same result with minutes and seconds in input.', 'test.peakforest.mass.rt.match.with.different.units', conn = conn)
