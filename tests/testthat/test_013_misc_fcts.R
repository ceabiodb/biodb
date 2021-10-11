test_splitVersion <- function() {
    testthat::expect_identical(biodb:::splitVersion('0.1'),
        list(major=0L, minor=1L, patch=0L))
    testthat::expect_identical(biodb:::splitVersion('0.1.1'),
        list(major=0L, minor=1L, patch=1L))
    testthat::expect_identical(biodb:::splitVersion('1.4.9'),
        list(major=1L, minor=4L, patch=9L))
    testthat::expect_identical(biodb:::splitVersion('1.4.10'),
        list(major=1L, minor=4L, patch=10L))
}

test_compareVersions <- function() {
    testthat::expect_equal(biodb:::compareVersions('0.1', '0.1'), 0)
    testthat::expect_equal(biodb:::compareVersions('0.1', '0.2'), -1)
    testthat::expect_equal(biodb:::compareVersions('0.2', '0.1'), +1)
    testthat::expect_equal(biodb:::compareVersions('0.9', '0.10'), -1)
    testthat::expect_equal(biodb:::compareVersions('0.10', '0.9'), +1)
    testthat::expect_equal(biodb:::compareVersions('0.1.1', '0.1.1'), 0)
    testthat::expect_equal(biodb:::compareVersions('1.4.9', '1.4.10'), -1)
    testthat::expect_equal(biodb:::compareVersions('1.4.9', '1.4.9'), 0)
    testthat::expect_equal(biodb:::compareVersions('1.4.10', '1.4.9'), +1)
}

# Main
################################################################################

# Set context
biodb::testContext("Testing miscellaneous functions")

# Run tests
biodb::testThat("We can split version string.", test_splitVersion)
biodb::testThat("We can compare string versions.", test_compareVersions)
