test_chebiex_getEntryPageUrl <- function(biodb) {

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
    conn.chebi <- biodb$getFactory()$createConn('chebi.ex')
    testthat::expect_is(conn.chebi, 'BiodbConn')
    
    myUrl <- conn.chebi$getEntryPageUrl('17001')
    testthat::expect_is(myUrl, 'character')
    testthat::expect_true(length(myUrl) == 1)
    testthat::expect_true( ! is.na(myUrl))
}

test_chebiex_getEntryImageUrl <- function(biodb) {

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
    conn.chebi <- biodb$getFactory()$createConn('chebi.ex')
    testthat::expect_is(conn.chebi, 'BiodbConn')
    
    myUrl <- conn.chebi$getEntryImageUrl('17001')
    testthat::expect_is(myUrl, 'character')
    testthat::expect_true(length(myUrl) == 1)
    testthat::expect_true( ! is.na(myUrl))
}

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance()

# Set context
biodb::testContext("Test chebi.ex connector.")

# Run tests
biodb::testThat("getEntryPageUrl() works.", test_chebiex_getEntryPageUrl, biodb)
biodb::testThat("getEntryImageUrl() works.", test_chebiex_getEntryImageUrl,
                biodb)

# Terminate Biodb
biodb$terminate()
