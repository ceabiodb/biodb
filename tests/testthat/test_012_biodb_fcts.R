test_connNameToClassPrefix <- function() {
    testthat::expect_equal("", biodb:::connNameToClassPrefix(""))
    testthat::expect_equal("Foo", biodb:::connNameToClassPrefix("foo"))
    testthat::expect_equal("FooDb", biodb:::connNameToClassPrefix("foo.db"))
}

test_extractVersion <- function() {
    outputDir <- file.path(getwd(), 'output')
    if ( ! dir.exists(outputDir))
        dir.create(outputDir)
    testfile <- file.path(outputDir, 'extractVersion_testfile.txt')
    writeLines('# blablabla version: 1.2', testfile)
    v <- extractVersion(testfile)
    testthat::expect_equal("1.2", v)
    writeLines('# blablabla version 1.2', testfile)
    v <- extractVersion(testfile)
    testthat::expect_equal("1.2", v)
    writeLines('# blablabla version: 1.2.3', testfile)
    v <- extractVersion(testfile)
    testthat::expect_equal("1.2.3", v)
}

# Main
################################################################################

# Set context
biodb::testContext("Testing biodb functions")

# Run tests
biodb::testThat("We can convert a connector name into a class prefix.",
                test_connNameToClassPrefix)
biodb::testThat("We can extract the version number from a template file.",
                test_extractVersion)
