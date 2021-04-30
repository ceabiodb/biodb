MASSFILEDB.URL <- system.file("extdata", "massbank_extract_full.tsv",
                              package="biodb")
COMPFILEDB.URL <- system.file("extdata", "chebi_extract.tsv", package="biodb")

# Generated files
MASS.SQLITE.URL <- file.path(system.file("extdata", package="biodb"),
                             'generated', "massbank_extract_full.sqlite")
COMP.SQLITE.URL <- file.path(system.file("extdata", package="biodb"),
                             'generated', "chebi_extract.sqlite")

test_createCompSQLiteDbFromCsvFile <- function(biodb) {

    testthat::expect_false(file.exists(COMP.SQLITE.URL))

    # Create folder
    folder <- dirname(COMP.SQLITE.URL)
    if ( ! dir.exists(folder))
        dir.create(folder, recursive=TRUE)
    
    # Create connectors
    csvConn <- biodb$getFactory()$createConn('comp.csv.file',
                                             url=COMPFILEDB.URL,
                                             fail.if.exists=FALSE)
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

    testthat::expect_false(file.exists(MASS.SQLITE.URL))

    # Create folder
    folder <- dirname(MASS.SQLITE.URL)
    if ( ! dir.exists(folder))
        dir.create(folder, recursive=TRUE)

    # Create connectors
    csvConn <- biodb$getFactory()$createConn('mass.csv.file',
                                             url=MASSFILEDB.URL,
                                             fail.if.exists=FALSE)
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

