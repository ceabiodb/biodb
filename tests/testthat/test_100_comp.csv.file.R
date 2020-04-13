CHEBI_FILE <- system.file("extdata", "chebi_extract.tsv", package="biodb")
UNIPROT_FILE <- system.file("extdata", "uniprot_extract.tsv", package="biodb")

# MAIN

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance(log='compcsvfile_test.log')

# Set context
biodb::setTestContext(biodb, "Test Compound CSV File connector.")

# TODO How to test this connector with both chebi and uniprot extracts?
# All entry-*.json are named after the connector name.
# Make runGenericTests() a class in which we can set the files pattern to match.
# Then listTestRefEntries() uses this pattern.

# Create connector
conn <- biodb$getFactory()$createConn('comp.csv.file', url=CHEBI_FILE)

# Run tests
biodb::runGenericTests(conn)

# Terminate Biodb
biodb$terminate()
