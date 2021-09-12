CHEBI_FILE <- system.file("extdata", "chebi_extract.tsv", package="biodb")

# Set context
biodb::testContext("CompCsvFile long generic tests")

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance()

# Create connector
conn <- biodb$getFactory()$createConn('comp.csv.file', url=CHEBI_FILE)

# Run generic tests
biodb::runGenericTests(conn, pkgName='biodb', short=FALSE, long=TRUE)

# Terminate Biodb
biodb$terminate()
