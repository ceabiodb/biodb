MASSFILEDB.URL <- system.file("extdata", "massbank_extract_lcms_1.tsv",
                              package="biodb")

test.connectorAlreadyExistsMassCsvFile <- function(biodb) {

    biodb$getFactory()$deleteAllConnectors()
    testthat::expect_length(biodb$getFactory()$getAllConnectors(), 0)
    testthat::expect_length(biodb$getRequestScheduler()$getAllRules(), 0)

    # Create a connector
    conn <- biodb$getFactory()$createConn('mass.csv.file', url=MASSFILEDB.URL)
    
    # Trying to create the same connector should fail
    testthat::expect_error(biodb$getFactory()$createConn('mass.csv.file',
                                                         url=MASSFILEDB.URL),
        regexp='.*A connector.*mass.csv.file.*already exists.*same URL.*')

    # Submitting an identical URL written differently should fail
    same.url <- normalizePath(MASSFILEDB.URL, mustWork=FALSE)
    same.url <- file.path(dirname(dirname(same.url)), '..',
                          basename(dirname(dirname(same.url))),
                          basename(dirname(same.url)), basename(same.url))
    testthat::expect_error(biodb$getFactory()$createConn('mass.csv.file',
                                                         url=same.url),
        regexp='A connector.*mass\\.csv\\.file.*already exists for database.*')
    
    # Now we force the creation of a connector with the same URL
    testthat::expect_warning(conn.2 <-
        biodb$getFactory()$createConn('mass.csv.file', url=same.url,
                                      fail.if.exists=FALSE),
        regexp='A connector.*mass\\.csv\\.file.*already exists for database.*')
    testthat::expect_is(conn.2, 'BiodbConn')

    biodb$getFactory()$deleteAllConnectors()
    testthat::expect_length(biodb$getFactory()$getAllConnectors(), 0)
    testthat::expect_length(biodb$getRequestScheduler()$getAllRules(), 0)
}

test.connectorAlreadyExistsChebiEx <- function(biodb) {

    biodb$getFactory()$deleteAllConnectors()
    testthat::expect_length(biodb$getFactory()$getAllConnectors(), 0)
    testthat::expect_length(biodb$getRequestScheduler()$getAllRules(), 0)

    # Test with ChEBI
    defFile <- system.file("extdata", "chebi_ex.yml", package="biodb")
    connFile <- system.file("extdata", "ChebiExConn.R", package="biodb")
    entryFile <- system.file("extdata", "ChebiExEntry.R", package="biodb")
    biodb$loadDefinitions(defFile)
    source(connFile)
    source(entryFile)
    conn.chebi <- biodb$getFactory()$createConn('chebi.ex')
    testthat::expect_is(conn.chebi, 'BiodbConn')
    testthat::expect_error(biodb$getFactory()$createConn('chebi.ex'),
        regexp='.*A connector.*chebi\\.ex.*already exists for database.*')

    testthat::expect_warning(conn.chebi.2 <-
        biodb$getFactory()$createConn('chebi.ex', fail.if.exists=FALSE),
        regexp='.*A connector.*chebi.ex.*already exists for database.*')
    testthat::expect_is(conn.chebi.2, 'BiodbConn')

    biodb$getFactory()$deleteAllConnectors()
    testthat::expect_length(biodb$getFactory()$getAllConnectors(), 0)
    testthat::expect_length(biodb$getRequestScheduler()$getAllRules(), 0)
}

test.connectorDeletion <- function(biodb) {

    biodb$getFactory()$deleteAllConnectors()
    testthat::expect_length(biodb$getFactory()$getAllConnectors(), 0)
    testthat::expect_length(biodb$getRequestScheduler()$getAllRules(), 0)

    # Load ChEBI connector definition
    defFile <- system.file("extdata", "chebi_ex.yml", package="biodb")
    connFile <- system.file("extdata", "ChebiExConn.R", package="biodb")
    entryFile <- system.file("extdata", "ChebiExEntry.R", package="biodb")
    biodb$loadDefinitions(defFile)
    source(connFile)
    source(entryFile)

    # Create more than one connector
    chebi.1 <- biodb$getFactory()$createConn('chebi.ex')
    testthat::expect_is(chebi.1, 'BiodbConn')
    testthat::expect_warning(chebi.2 <-
        biodb$getFactory()$createConn('chebi.ex', fail.if.exists=FALSE),
        regex='.*A connector.*chebi.ex.*already exists.*')
    testthat::expect_is(chebi.2, 'BiodbConn')
    chebi.3 <- biodb$getFactory()$createConn('chebi.ex',
                                             url='http://some.fake.chebi.site/')
    testthat::expect_is(chebi.3, 'BiodbConn')

    # Delete all connectors
    testthat::expect_true(length(biodb$getFactory()$getAllConnectors()) >= 2)
    testthat::expect_true(length(biodb$getRequestScheduler()$getAllRules())
                          >= 1)
    biodb$getFactory()$deleteAllConnectors()
    testthat::expect_length(biodb$getFactory()$getAllConnectors(), 0)
    testthat::expect_length(biodb$getRequestScheduler()$getAllRules(), 0)
}


test.connectorDefaultValues <- function(biodb) {

    biodb$getFactory()$deleteAllConnectors()
    testthat::expect_length(biodb$getFactory()$getAllConnectors(), 0)
    testthat::expect_length(biodb$getRequestScheduler()$getAllRules(), 0)

    defFile <- system.file("extdata", "chebi_ex.yml", package="biodb")
    connFile <- system.file("extdata", "ChebiExConn.R", package="biodb")
    entryFile <- system.file("extdata", "ChebiExEntry.R", package="biodb")
    biodb$loadDefinitions(defFile)
    source(connFile)
    source(entryFile)
    chebi <- biodb$getFactory()$createConn('chebi.ex')
    chebi.info <- biodb$getDbsInfo()$get('chebi.ex')
    testthat::expect_equal(chebi.info$getPropValSlot('urls', 'base.url'),
                           chebi$getPropValSlot('urls', 'base.url'))
    testthat::expect_equal(chebi.info$getPropertyValue('scheduler.n'),
                           chebi$getPropertyValue('scheduler.n'))
    testthat::expect_equal(chebi.info$getPropertyValue('scheduler.t'),
                           chebi$getPropertyValue('scheduler.t'))

    biodb$getFactory()$deleteAllConnectors()
    testthat::expect_length(biodb$getFactory()$getAllConnectors(), 0)
    testthat::expect_length(biodb$getRequestScheduler()$getAllRules(), 0)
}

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance()

# Set context
biodb::testContext("Test BiodbFactory.")

# Run tests
biodb::testThat("We forbid identical connectors (MassCsvFile test).",
                test.connectorAlreadyExistsMassCsvFile, biodb=biodb)
biodb::testThat("We forbid identical connectors (ChebiEx test).",
                test.connectorAlreadyExistsChebiEx, biodb=biodb)
biodb::testThat("A newly created connector get the default values.",
                test.connectorDefaultValues, biodb=biodb)
biodb::testThat("Connectors are deleted.", test.connectorDeletion, biodb=biodb)

# Terminate Biodb
biodb$terminate()
