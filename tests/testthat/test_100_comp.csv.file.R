CHEBI_FILE <- system.file("extdata", "chebi_extract.tsv", package="biodb")
CHEBI_FILE_WITH_UNKNOWN_COL <- system.file("extdata", "chebi_extract_with_unknown_column.tsv", package="biodb")

test.comp.csv.file.dynamic.field.set <- function(biodb) {
    conn <- biodb$getFactory()$createConn('comp.csv.file', url=CHEBI_FILE_WITH_UNKNOWN_COL)
    x <- conn$getEntry('1932')$getFieldsAsDataframe()
    testthat::expect_false('charge' %in% colnames(x))
    conn$setField('charge', 'elecCharge')
    conn$deleteAllEntriesFromPersistentCache() # We must first remove the entry from all caches
    x <- conn$getEntry('1932')$getFieldsAsDataframe()
    testthat::expect_true('charge' %in% colnames(x))
}

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
biodb::testThat('We can define a new field even after loading an entry.', test.comp.csv.file.dynamic.field.set, biodb=biodb)

# Terminate Biodb
biodb$terminate()
