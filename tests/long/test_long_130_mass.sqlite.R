source(file.path(getwd(), '..', 'db_creation.R'))

# Set context
biodb::testContext("MassSqlite long generic tests")

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance()

# Create a Mass SQLite database from a Mass CSV file
if ( ! file.exists(MASS.SQLITE.URL))
    biodb::testThat('We can create an SQLite mass db from a Mass CSV file.',
                    test_createMassSQLiteDbFromCsvFile, biodb=biodb)

# Create connector
conn <- biodb$getFactory()$createConn('mass.sqlite', url=MASS.SQLITE.URL)

# Run generic tests only if DB file has been created.
if (file.exists(MASS.SQLITE.URL))
    biodb::runGenericTests(conn, short=FALSE, long=TRUE)

# Terminate Biodb
biodb$terminate()
