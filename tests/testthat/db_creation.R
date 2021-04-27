MASSFILEDB.URL <- system.file("extdata", "massbank_extract_full.tsv",
                              package="biodb")
MASS.SQLITE.URL <- file.path(dirname(MASSFILEDB.URL),
                             "massbank_extract_full.sqlite")
COMPFILEDB.URL <- system.file("extdata", "chebi_extract.tsv", package="biodb")
COMP.SQLITE.URL <- file.path(dirname(COMPFILEDB.URL),
                             'chebi_extract.sqlite')

test_createCompSQLiteDbFromCsvFile <- function(biodb) {

    # Create connectors
    csvConn <- biodb$getFactory()$createConn('comp.csv.file',
                                             url=COMPFILEDB.URL,
                                             fail.if.exists=FALSE)
    testthat::expect_false(file.exists(COMP.SQLITE.URL))
    sqlConn <- biodb$getFactory()$createConn('comp.sqlite', url=COMP.SQLITE.URL)
    testthat::expect_identical(character(), sqlConn$getEntryIds())

    # Make sure we have no residual cache entries from previous tests
    biodb$getPersistentCache()$deleteAllFiles(sqlConn$getCacheId(), fail=FALSE)

    # Check CSV conn
    ids <- csvConn$getEntryIds()
    testthat::expect_is(ids, 'character')
    testthat::expect_true(length(ids) > 0)
    testthat::expect_false(any(is.na(ids)))

    # Copy CSV db into SQLite db
    sqlConn$allowEditing()
    biodb$copyDb(csvConn, sqlConn)
    sqlConn$allowWriting()
    sqlConn$write()

    # Test
    testthat::expect_identical(csvConn$getEntryIds(), sqlConn$getEntryIds())

    # Delete connectors
    biodb$getFactory()$deleteConn(csvConn)
    biodb$getFactory()$deleteConn(sqlConn)

    testthat::expect_true(file.exists(COMP.SQLITE.URL))
}

test_createMassSQLiteDbFromCsvFile <- function(biodb) {

    # Create connectors
    csvConn <- biodb$getFactory()$createConn('mass.csv.file',
                                             url=MASSFILEDB.URL,
                                             fail.if.exists=FALSE)
    testthat::expect_false(file.exists(MASS.SQLITE.URL))
    sqlConn <- biodb$getFactory()$createConn('mass.sqlite', url=MASS.SQLITE.URL)
    testthat::expect_identical(character(), sqlConn$getEntryIds())

    # Make sure we have no residual cache entries from previous tests
    biodb$getPersistentCache()$deleteAllFiles(sqlConn$getCacheId(), fail=FALSE)

    # Check CSV conn
    ids <- csvConn$getEntryIds()
    testthat::expect_is(ids, 'character')
    testthat::expect_true(length(ids) > 0)
    testthat::expect_false(any(is.na(ids)))

    # Copy CSV db into SQLite db
    sqlConn$allowEditing()
    biodb$copyDb(csvConn, sqlConn)
    sqlConn$allowWriting()
    sqlConn$write()

    # Test
    testthat::expect_identical(csvConn$getEntryIds(), sqlConn$getEntryIds())

    # Delete connectors
    biodb$getFactory()$deleteConn(csvConn)
    biodb$getFactory()$deleteConn(sqlConn)

    testthat::expect_true(file.exists(MASS.SQLITE.URL))
}

