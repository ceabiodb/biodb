test_range <- function() {
    rng <- Range$new(min=0, max=1)
    testthat::expect_equal(0.5, rng$getValue())
    testthat::expect_equal(0.5, rng$getDelta())
    rng <- Range$new(min=1-1e-6, max=1+1e-6)
    testthat::expect_equal(1, rng$getValue())
    testthat::expect_equal(1, rng$getPpm())
}

# Set context
biodb::testContext("Testing Range class.")

# Run tests
biodb::testThat("Range class works correctly.", test_range)

