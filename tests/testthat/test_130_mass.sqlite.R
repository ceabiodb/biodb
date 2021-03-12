MASS.SQLITE.URL <- file.path(getwd(), 'output', 'massSqliteDbFromCsvFile.sqlite')
MASSFILEDB.URL <- file.path(getwd(), 'res', 'mass.csv.file.tsv')

test_createSQLiteDbFromCsvFile <- function(biodb) {

    # Create connectors
	csvConn <- biodb$getFactory()$createConn('mass.csv.file',
                                             url=MASSFILEDB.URL,
                                             fail.if.exists=FALSE)
    csvConn$setField('accession', c('compound.id', 'ms.mode', 'chrom.col.name',
                                    'chrom.rt'))
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

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance(log='masssqlite_test.log')

# Set context
biodb::setTestContext(biodb, "Test Mass SQLite connector.")

# Remove DB file
if (file.exists(MASS.SQLITE.URL))
    unlink(MASS.SQLITE.URL)
testthat::expect_false(file.exists(MASS.SQLITE.URL))

# Create a Mass SQLite database from a Mass CSV file
biodb::testThat('We can create an SQLite mass database from a Mass CSV file.',
                test_createSQLiteDbFromCsvFile, biodb=biodb)

# Create connector
conn <- biodb$getFactory()$createConn('mass.sqlite', url=MASS.SQLITE.URL)

# Run generic tests only if DB file has been created.
if (file.exists(MASS.SQLITE.URL))
    biodb::runGenericTests(conn)

# Terminate Biodb
biodb$terminate()
