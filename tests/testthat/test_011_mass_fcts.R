test_convertTolToRange <- function() {

	# Wrong type
	testthat::expect_error(convertTolToRange(1.0, 0.0, 'notatype'))

	# Test plain unit
	rng <- convertTolToRange(1.0, 0.0, 'plain')
	testthat::expect_is(rng, 'list')
	testthat::expect_identical(rng, list(a=1.0, b=1.0))

	# Test plain unit
	rng <- convertTolToRange(1.0, 0.1, 'plain')
	testthat::expect_is(rng, 'list')
	testthat::expect_identical(rng, list(a=0.9, b=1.1))

	# Test ppm unit
	rng <- convertTolToRange(1.0, 0.0, 'ppm')
	testthat::expect_is(rng, 'list')
	testthat::expect_identical(rng, list(a=1.0, b=1.0))

	# Test ppm unit
	rng <- convertTolToRange(1.0, 0.1, 'ppm')
	testthat::expect_is(rng, 'list')
	testthat::expect_identical(rng, list(a=1.0-1.0*0.1*1e-6, b=1.0+1.0*0.1*1e-6))

	# Test NA value
	rng <- convertTolToRange(NA_real_, 0.0, 'ppm')
	testthat::expect_is(rng, 'list')
	testthat::expect_identical(rng, list(a=NA_real_, b=NA_real_))
}

test_closeMatchPpm <- function() {
    testthat::expect_equal(closeMatchPpm(0, 0, 1L, 1L, 1L, 0, 0),
                            list(1))
    testthat::expect_equal(closeMatchPpm(1, 1, 1L, 1L, 1L, 0, 0),
                            list(1))
    testthat::expect_equal(closeMatchPpm(0, 1, 1L, 1L, 1L, 0, 0),
                            list(NULL))
    testthat::expect_equal(closeMatchPpm(10, 10, 1L, 1L, 1L, 0, 0),
                            list(1))
    testthat::expect_equal(closeMatchPpm(5, 5, 1L, 1L, 1L, 0, 0),
                            list(1))
    testthat::expect_equal(closeMatchPpm(c(5, 10), c(5, 10), c(1L, 2L), c(1L, 2L), 2L, 0, 0),
                            list(1, 2))
    testthat::expect_equal(closeMatchPpm(c(5, 10), c(1, 9), c(1L, 2L), c(1L, 2L), 2L, 0, 0),
                            list(NULL, NULL))
    testthat::expect_equal(closeMatchPpm(c(286.1456, 287.1488, 288.1514), c(201.0918, 211.0760, 229.0869, 268.1343, 286.1457, 287.1489, 288.1513), c(1L, 2L, 3L), c(5L, 7L, 3L, 6L, 1L, 2L, 4L), 3L, 3.0, 0.005),
                            list(1, 2, 4))
}

# Main
################################################################################

# Set context
biodb::testContext("Testing mass functions")

# Run tests
biodb::testThat("Conversion from tolerance to range works correctly.",
                test_convertTolToRange)
biodb::testThat("closeMatchPpm() works correctly.", test_closeMatchPpm)
