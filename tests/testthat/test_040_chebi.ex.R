test_chebiex_getEntryPageUrl <- function(conn) {
    
    myUrl <- conn$getEntryPageUrl('17001')
    testthat::expect_is(myUrl, 'character')
    testthat::expect_true(length(myUrl) == 1)
    testthat::expect_true( ! is.na(myUrl))
}

test_chebiex_getEntryImageUrl <- function(conn) {
    
    myUrl <- conn$getEntryImageUrl('17001')
    testthat::expect_is(myUrl, 'character')
    testthat::expect_true(length(myUrl) == 1)
    testthat::expect_true( ! is.na(myUrl))
}

test_getEntry <- function(conn) {

    entry <- conn$getEntry('17001')
    testthat::expect_is(entry, 'BiodbEntry')
    conn$deleteAllEntriesFromVolatileCache()
    entry <- conn$getEntry('17001')
    testthat::expect_is(entry, 'BiodbEntry')
}

# Set context
biodb::testContext("Test chebi.ex connector.")

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance()

# Delete existing connectors
biodb$getFactory()$deleteAllConnectors()
testthat::expect_length(biodb$getFactory()$getAllConnectors(), 0)
testthat::expect_length(biodb$getRequestScheduler()$.getAllRules(), 0)

# Test with ChEBI
defFile <- system.file("extdata", "chebi_ex.yml", package="biodb")
connFile <- system.file("extdata", "ChebiExConn.R", package="biodb")
entryFile <- system.file("extdata", "ChebiExEntry.R", package="biodb")
biodb$loadDefinitions(defFile)
source(connFile)
source(entryFile)
conn <- biodb$getFactory()$createConn('chebi.ex')
testthat::expect_is(conn, 'BiodbConn')

# Run tests
biodb::testThat("getEntryPageUrl() works.", test_chebiex_getEntryPageUrl,
    conn=conn)
biodb::testThat("getEntryImageUrl() works.", test_chebiex_getEntryImageUrl,
    conn=conn)
biodb::testThat("We can retrieve entries.", test_getEntry, conn=conn)

# Terminate Biodb
biodb$terminate()
