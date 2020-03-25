CHEBI_FILE <- system.file("extdata", "chebi_extract.tsv", package="biodb")

# MAIN

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance(log='compcsvfile_test.log')

# Set context
biodb::setTestContext(biodb, "Test Compound CSV File connector.")

# Create connector
conn <- biodb$getFactory()$createConn('comp.csv.file', url=CHEBI_FILE)

# Run tests
biodb::runGenericTests(conn)

# Terminate Biodb
biodb$terminate()
