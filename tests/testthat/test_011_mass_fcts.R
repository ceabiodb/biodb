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

# Main
################################################################################

# Set context
biodb::testContext("Testing mass functions")

# Run tests
biodb::testThat("Conversion from tolerance to range works correctly.", test_convertTolToRange)

