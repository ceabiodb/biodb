COMP.SQLITE.URL <- file.path(getwd(), 'output',
                             'compSqliteDbFromCsvFile.sqlite')
COMPFILEDB.URL <- system.file("extdata", "chebi_extract.tsv", package="biodb")

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

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance()

# Set context
biodb::testContext("Test Compound SQLite connector.")

# Remove DB file
if (file.exists(COMP.SQLITE.URL))
    unlink(COMP.SQLITE.URL)
testthat::expect_false(file.exists(COMP.SQLITE.URL))

# Create a Compound SQLite database from a Mass CSV file
biodb::testThat('We can create an SQLite comp db from a Compound CSV file.',
                test_createCompSQLiteDbFromCsvFile, biodb=biodb)

# Create connector
conn <- biodb$getFactory()$createConn('comp.sqlite', url=COMP.SQLITE.URL)

# Run generic tests only if DB file has been created.
if (file.exists(COMP.SQLITE.URL))
    biodb::runGenericTests(conn)

# Terminate Biodb
biodb$terminate()
