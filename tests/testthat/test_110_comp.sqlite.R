source(system.file("testsrc", "db_creation.R", package="biodb"))

# Set context
biodb::testContext("CompSqlite generic tests")

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance()

# Create a Compound SQLite database from a Mass CSV file
if ( ! file.exists(COMP.SQLITE.URL))
    biodb::testThat('We can create an SQLite comp db from a Compound CSV file.',
                test_createCompSQLiteDbFromCsvFile, biodb=biodb)

# Create connector
conn <- biodb$getFactory()$createConn('comp.sqlite', url=COMP.SQLITE.URL)

# Run generic tests only if DB file has been created.
if (file.exists(COMP.SQLITE.URL))
    biodb::runGenericTests(conn, pkgName='biodb')

# Terminate Biodb
biodb$terminate()
