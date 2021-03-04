CHEBI_FILE <- system.file("extdata", "chebi_extract.tsv", package="biodb")
CHEBI_FILE_UNKNOWN_COL <- system.file("extdata",
                                      "chebi_extract_with_unknown_column.tsv",
                                      package="biodb")

test.comp.csv.file.dynamic.field.set <- function(biodb) {

    # Create connector
    conn <- biodb$getFactory()$createConn('comp.csv.file',
                                          url=CHEBI_FILE_UNKNOWN_COL)
    
    msg <- "^.* Column \"elecCharge\" does not match any biodb field\\.$"
    testthat::expect_warning(x <- conn$getEntry('1932')$getFieldsAsDataframe(),
                             msg, perl=TRUE)
    testthat::expect_false('charge' %in% colnames(x))
    conn$setField('charge', 'elecCharge')
    
    # We must first remove the entry from all caches
    conn$deleteAllEntriesFromPersistentCache()

    x <- conn$getEntry('1932')$getFieldsAsDataframe()
    testthat::expect_true('charge' %in% colnames(x))
    
    # Delete connector
    biodb$getFactory()$deleteConn(conn)
}

test_unmapped_col <- function(biodb, obs) {

    # Create connector
    conn <- biodb$getFactory()$createConn('comp.csv.file',
                                          url=CHEBI_FILE_UNKNOWN_COL)
    
    obs$clearMessages();
    msg <- "^.* Column \"elecCharge\" does not match any biodb field\\.$"
    testthat::expect_warning(conn$getEntryIds(), msg, perl=TRUE)
    testthat::expect_true(obs$hasMsgs())

    # Re-create connector
    biodb$getFactory()$deleteConn(conn)
    conn <- biodb$getFactory()$createConn('comp.csv.file',
                                          url=CHEBI_FILE_UNKNOWN_COL)

    # Re-create connector
    biodb$getFactory()$deleteConn(conn)
    conn <- biodb$getFactory()$createConn('comp.csv.file',
                                          url=CHEBI_FILE_UNKNOWN_COL)
    
    conn$ignoreUnassignedColumns()
    conn$getEntryIds()

    # Define missing column
    conn$setField('charge', 'elecCharge')

    # No warning should be issued
    obs$clearMessages()
    conn$getEntryIds()
    testthat::expect_false(obs$hasMsgs('warning'))
    
    # Delete connector
    biodb$getFactory()$deleteConn(conn)
}

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance(log='compcsvfile_test.log')

# Set context
biodb::setTestContext(biodb, "Test Compound CSV File connector.")
obs <- biodb::addMsgRecObs(biodb)

# TODO How to test this connector with both chebi and uniprot extracts?
# All entry-*.json are named after the connector name.
# Make runGenericTests() a class in which we can set the files pattern to match.
# Then listTestRefEntries() uses this pattern.

# Create connector
conn <- biodb$getFactory()$createConn('comp.csv.file', url=CHEBI_FILE)

# Run tests
biodb::runGenericTests(conn)
biodb::testThat('We receive a warning for unmapped columns.',
                test_unmapped_col, biodb=biodb, obs=obs)
biodb::testThat('We can define a new field even after loading an entry.',
                test.comp.csv.file.dynamic.field.set, biodb=biodb)

# Terminate Biodb
biodb$terminate()
