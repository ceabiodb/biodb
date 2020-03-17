# vi: fdm=marker

MASSFILEDB.URL <- file.path(getwd(), 'res', 'mass.csv.file.tsv')

# Test connector already exists {{{1
################################################################

test.connectorAlreadyExists <- function(biodb, obs) {

	# Test with Mass CSV file
	conn <- biodb$getFactory()$createConn('mass.csv.file', url = MASSFILEDB.URL)
	testthat::expect_error(biodb$getFactory()$createConn('mass.csv.file', url = MASSFILEDB.URL))

	# Make change to path
	same.url <- normalizePath(MASSFILEDB.URL, mustWork = FALSE)
	same.url <- file.path(dirname(dirname(same.url)), '..', basename(dirname(dirname(same.url))), basename(dirname(same.url)), basename(same.url))
	testthat::expect_error(biodb$getFactory()$createConn('mass.csv.file', url = same.url))
	expect_match(obs$getLastMsg(), '^A connector \\(mass\\.csv\\.file\\) already exists for database mass\\.csv\\.file .*$', perl = TRUE)
	conn.2 <- biodb$getFactory()$createConn('mass.csv.file', url = same.url, fail.if.exists = FALSE)
	expect_is(conn.2, 'BiodbConn')
	expect_match(obs$getLastMsgByType('caution'), "A connector \\(mass\\.csv\\.file\\) already exists for database mass\\.csv\\.file with the same URL ", , perl = TRUE)

	# Test with ChEBI
	biodb$getFactory()$deleteConnByClass('chebi')
	conn.chebi <- biodb$getFactory()$createConn('chebi')
	testthat::expect_error(biodb$getFactory()$createConn('chebi'))
	expect_match(obs$getLastMsg(), "^A connector \\(chebi\\) already exists for database chebi with the same URL .*$", perl = TRUE)
	conn.chebi.2 <- biodb$getFactory()$createConn('chebi', fail.if.exists = FALSE)
	expect_is(conn.chebi.2, 'BiodbConn')
	expect_match(obs$getLastMsgByType('caution'), "^A connector \\(chebi\\) already exists for database chebi with the same URL .*$", perl = TRUE)
}

# Test connector deletion {{{1
################################################################

test.connectorDeletion <- function(biodb, obs) {

	biodb$getFactory()$deleteAllConnectors()

	# Create more than one connector
	chebi.1 <- biodb$getFactory()$createConn('chebi')
	chebi.2 <- biodb$getFactory()$createConn('chebi', fail.if.exists = FALSE)

	# Delete all connectors
	testthat::expect_true(length(biodb$getFactory()$getAllConnectors()) >= 2)
	testthat::expect_true(length(biodb$getRequestScheduler()$.getAllRules()) >= 1)
	biodb$getFactory()$deleteAllConnectors()
	testthat::expect_length(biodb$getFactory()$getAllConnectors(), 0)
	testthat::expect_length(biodb$getRequestScheduler()$.getAllRules(), 0)
}

# Test connector default values {{{1
################################################################

test.connectorDefaultValues <- function(biodb, obs) {

	biodb$getFactory()$deleteAllConnectors()
	chebi <- biodb$getFactory()$createConn('chebi')
	chebi.info <- biodb$getDbsInfo()$get('chebi')
	testthat::expect_equal(chebi.info$getBaseUrl(), chebi$getBaseUrl())
	testthat::expect_equal(chebi.info$getSchedulerNParam(), chebi$getSchedulerNParam())
	testthat::expect_equal(chebi.info$getSchedulerTParam(), chebi$getSchedulerTParam())
}

# Main {{{1
################################################################

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance(log='biodb_test.log')
obs <- biodb::addMsgRecObs(biodb)

# Set context
biodb::setTestContext(biodb, "Test BiodbFactory.")

# Run tests
biodb::testThat("We detect when an identical connector already exists.", test.connectorAlreadyExists, biodb = biodb, obs = obs)
biodb::testThat("A newly created connector get the default values.", test.connectorDefaultValues, biodb = biodb, obs = obs)
biodb::testThat("Connectors are deleted.", test.connectorDeletion, biodb = biodb, obs = obs)

# Terminate Biodb
biodb$terminate()
