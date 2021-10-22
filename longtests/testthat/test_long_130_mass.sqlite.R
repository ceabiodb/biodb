source(system.file("testsrc", "db_creation.R", package="biodb"))

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
    biodb::runGenericTests(conn, pkgName='biodb', short=FALSE, long=TRUE)

# Terminate Biodb
biodb$terminate()
