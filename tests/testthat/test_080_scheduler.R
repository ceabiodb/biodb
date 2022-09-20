test.schedulerRightRule <- function(biodb) {

    # Get scheduler
    scheduler <- biodb$getRequestScheduler()

    # Delete all connectors
    biodb$getFactory()$deleteAllConnectors()
    testthat::expect_true(all(vapply(scheduler$getAllRules(),
        function(rule) length(rule$getConnectors()), FUN.VALUE=0) == 0))

    # Load ChEBI connector definition
    defFile <- system.file("extdata", "chebi_ex.yml", package="biodb")
    connFile <- system.file("extdata", "ChebiExConn.R", package="biodb")
    entryFile <- system.file("extdata", "ChebiExEntry.R", package="biodb")
    biodb$loadDefinitions(defFile)
    source(connFile)
    source(entryFile)

    # Get ChEBI connector
    chebi <- biodb$getFactory()$createConn('chebi.ex')

    # Get connector rule
    rules <- scheduler$getConnectorRules(chebi)
    testthat::expect_is(rules, 'list')
    testthat::expect_length(rules, 1)
    testthat::expect_is(rules[[1]], 'BiodbRequestSchedulerRule')
    testthat::expect_length(rules[[1]]$getConnectors(), 1)
    testthat::expect_identical(rules[[1]]$getConnectors()[[1]], chebi)
    testthat::expect_equal(rules[[1]]$getN(), chebi$getSchedulerNParam())
    testthat::expect_equal(rules[[1]]$getT(), chebi$getSchedulerTParam())

    # Delete connector
    biodb$getFactory()$deleteConn(chebi)
    testthat::expect_null(scheduler$getConnectorRules(chebi))
    rules <- scheduler$getAllRules()
    testthat::expect_is(rules, 'list')
    testthat::expect_length(rules, 0)
}

test.schedulerRuleFrequency <- function(biodb) {

    # Delete all connectors
    biodb$getFactory()$deleteAllConnectors()
    testthat::expect_length(biodb$getFactory()$getAllConnectors(), 0)

    # Get scheduler
    scheduler <- biodb$getRequestScheduler()
    testthat::expect_true(all(vapply(scheduler$getAllRules(),
        function(rule) length(rule$getConnectors()), FUN.VALUE=0) == 0))

    # Load ChEBI connector definition
    defFile <- system.file("extdata", "chebi_ex.yml", package="biodb")
    connFile <- system.file("extdata", "ChebiExConn.R", package="biodb")
    entryFile <- system.file("extdata", "ChebiExEntry.R", package="biodb")
    biodb$loadDefinitions(defFile)
    source(connFile)
    source(entryFile)

    # Get ChEBI connector
    chebi <- biodb$getFactory()$createConn('chebi.ex')
    chebi$setSchedulerNParam(3)
    chebi$setSchedulerTParam(1)

    # Get connector rule
    rules <- scheduler$getConnectorRules(chebi)
    testthat::expect_is(rules, 'list')
    testthat::expect_length(rules, 1)
    biodb::logDebug('Hosts: %s', biodb::lst2str(names(scheduler$.host2rule)))
    biodb::logDebug('Connector IDs: %s',
        biodb::lst2str(names(scheduler$.connid2rules)))
    rule <- rules[[1]]
    testthat::expect_is(rule, 'BiodbRequestSchedulerRule')
    testthat::expect_equal(rule$getN(), chebi$getSchedulerNParam())
    testthat::expect_equal(rule$getT(), chebi$getSchedulerTParam())

    # Create another ChEBI connector
    chebi.2 <- biodb$getFactory()$createConn('chebi.ex',
                                             url='http://some.fake.chebi.site/')
    biodb::logDebug('Hosts: %s', biodb::lst2str(names(scheduler$.host2rule)))
    biodb::logDebug('Connector IDs: %s',
        biodb::lst2str(names(scheduler$.connid2rules)))
    rules <- scheduler$getConnectorRules(chebi.2)
    testthat::expect_length(rules, 2)
    for (r in rules) { 
        testthat::expect_equal(r$getN(), chebi$getSchedulerNParam())
        testthat::expect_equal(r$getT(), chebi$getSchedulerTParam())
    }

    # Change frequency of second connector. Since the rule is in common between
    # the two connectors for the site default site, the frequency should change
    # only when decreasing but not when increasing.
    n <- rule$getN()
biodb::logDebug("test.schedulerRuleFrequency 100")
    chebi.2$setPropertyValue('scheduler.n', n + 1)
biodb::logDebug("test.schedulerRuleFrequency 101")
    testthat::expect_equal(rule$getN(), n)
biodb::logDebug("test.schedulerRuleFrequency 102")
    chebi.2$setPropertyValue('scheduler.n', n - 1)
biodb::logDebug("test.schedulerRuleFrequency 103")
    testthat::expect_equal(rule$getN(), n - 1)
biodb::logDebug("test.schedulerRuleFrequency 104")
    chebi.2$setPropertyValue('scheduler.n', n)
biodb::logDebug("test.schedulerRuleFrequency 105")
    testthat::expect_equal(rule$getN(), n)
biodb::logDebug("test.schedulerRuleFrequency 106")
    .t <- rule$getT()
    chebi.2$setPropertyValue('scheduler.t', .t + 0.5)
    testthat::expect_equal(rule$getT(), .t + 0.5)
    chebi.2$setPropertyValue('scheduler.t', .t - 0.5)
    testthat::expect_equal(rule$getT(), .t)
    chebi.2$setPropertyValue('scheduler.t', .t * 2)
    testthat::expect_equal(rule$getT(), .t * 2)
    chebi.2$setPropertyValue('scheduler.n', n * 2)
    testthat::expect_equal(rule$getT(), .t)
    testthat::expect_equal(rule$getN(), n)
    testthat::expect_equal(rule$getT(), .t)

    biodb$getFactory()$deleteAllConnectors()
    testthat::expect_length(biodb$getFactory()$getAllConnectors(), 0)
    testthat::expect_length(biodb$getRequestScheduler()$getAllRules(), 0)
}

test.schedulerSleepTime <- function(biodb) {

    n <- 3
    t <- 1.0

    # Delete all connectors
    biodb$getFactory()$deleteAllConnectors()

    # Get scheduler
    scheduler <- biodb$getRequestScheduler()

    # Load ChEBI connector definition
    defFile <- system.file("extdata", "chebi_ex.yml", package="biodb")
    connFile <- system.file("extdata", "ChebiExConn.R", package="biodb")
    entryFile <- system.file("extdata", "ChebiExEntry.R", package="biodb")
    biodb$loadDefinitions(defFile)
    source(connFile)
    source(entryFile)

    # Get ChEBI connector
    chebi <- biodb$getFactory()$getConn('chebi.ex')
    chebi$setSchedulerNParam(n)
    chebi$setSchedulerTParam(t)

    # Get connector rule
    rules <- scheduler$getConnectorRules(chebi)
    testthat::expect_is(rules, 'list')
    testthat::expect_length(rules, 1)
    rule <- rules[[1]]
    testthat::expect_is(rule, 'BiodbRequestSchedulerRule')

    # Test sleep time
    cur.time <- Sys.time()
    for (i in seq(n)) {
        tt <- cur.time + (i - 1) * t / 10
        testthat::expect_equal(rule$computeSleepTime(tt), 0)
        rule$storeCurrentTime(tt)
    }
    testthat::expect_equal(rule$computeSleepTime(cur.time), t)
    testthat::expect_true(abs(rule$computeSleepTime(cur.time + t - 0.1) - 0.1) < 1e-6)
    testthat::expect_equal(rule$computeSleepTime(cur.time + t), 0)
    rule$storeCurrentTime(cur.time + t)
    testthat::expect_true(abs(rule$computeSleepTime(cur.time + t) - t / 10) < 1e-6)
}

test_directRequestToChebi <- function(biodb) {

    # Delete all connectors
    biodb$getFactory()$deleteAllConnectors()

    # Get the scheduler
    sched <- biodb$getRequestScheduler()
    testthat::expect_is(sched, "BiodbRequestScheduler")

    # Create URL object
    u <- 'https://www.ebi.ac.uk/webservices/chebi/2.0/test/getCompleteEntity'
    url <- BiodbUrl$new(url=u, params=list(chebiId=15440))

    # Check rule does not exist
    testthat::expect_null(sched$findRule(url, create=FALSE))
    # ==> no connector is registered with this domain

    # Create a request object
    request <- BiodbRequest$new(method='get', url=url)

    # Send request
    result <- sched$sendRequest(request)
    testthat::expect_is(result, 'character')
    testthat::expect_length(result, 1)
    testthat::expect_false(is.na(result))
}

test_directRequestToUniprot <- function(biodb) {

    # Delete all connectors
    biodb$getFactory()$deleteAllConnectors()

    # Get the scheduler
    sched <- biodb$getRequestScheduler()
    testthat::expect_is(sched, "BiodbRequestScheduler")

    # Create URL object
    u <- 'https://rest.uniprot.org/uniprotkb/search'
    p <- list(query='e', fields='id', format='tsv', size=2)
    url <- BiodbUrl$new(url=u, params=p, chompExtraSlashes=FALSE)

    # Check rule does not exist
    testthat::expect_null(sched$findRule(url, create=FALSE))
    # ==> no connector is registered with this domain

    # Create a request object
    request <- BiodbRequest$new(method='get', url=url)

    # Send request
    result <- sched$sendRequest(request)
    testthat::expect_is(result, 'character')
    testthat::expect_length(result, 1)
    testthat::expect_false(is.na(result))
}

test_wrongURL <- function(biodb) {

    # Get the scheduler
    sched <- biodb$getRequestScheduler()
    testthat::expect_is(sched, "BiodbRequestScheduler")

    # Create URL object
    u <- 'http://rest.kegg.jp/get/mmu00627'
    url <- BiodbUrl$new(url=u)

    # Create a request object
    request <- BiodbRequest$new(method='get', url=url)

    # Send request
    result <- sched$sendRequest(request)
    testthat::expect_is(result, 'character')
    testthat::expect_length(result, 1)
    testthat::expect_true(is.na(result))
}

# Set context
biodb::testContext("Test scheduler.")

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance()

# Run tests
biodb::testThat("Right rule is created.", test.schedulerRightRule, biodb=biodb)
biodb::testThat("Frequency is updated correctly.", test.schedulerRuleFrequency,
    biodb=biodb)
biodb::testThat("Sleep time is computed correctly.", test.schedulerSleepTime,
    biodb=biodb)
biodb::testThat("We can send a direct request to ChEBI.",
    test_directRequestToChebi, biodb=biodb)
biodb::testThat("We can send a direct request to UniProt.",
    test_directRequestToUniprot, biodb=biodb)
biodb::testThat("We can handle correctly a wrong URL.", test_wrongURL,
    biodb=biodb)

# Terminate Biodb
biodb$terminate()
