# vi: fdm=marker

source('common.R', local=TRUE)
biodb <- create.biodb.instance()

# Set context
biodb::setTestContext(biodb, "Test scheduler.")

# Test right rule {{{1
################################################################

test.schedulerRightRule <- function(biodb) {

	# Delete all connectors
	biodb$getFactory()$deleteAllConnectors()

	# Get scheduler
	scheduler <- biodb$getRequestScheduler()

	# Get ChEBI connector
	chebi <- biodb$getFactory()$getConn('chebi')

	# Get connector rule
	rules <- scheduler$.getConnectorRules(chebi)
	testthat::expect_is(rules, 'list')
	testthat::expect_length(rules, 1)
	testthat::expect_is(rules[[1]], 'BiodbRequestSchedulerRule')
	testthat::expect_length(rules[[1]]$getConnectors(), 1)
	testthat::expect_identical(rules[[1]]$getConnectors()[[1]], chebi)
	testthat::expect_equal(rules[[1]]$getN(), chebi$getSchedulerNParam())
	testthat::expect_equal(rules[[1]]$getT(), chebi$getSchedulerTParam())
}

# Test rule frequency {{{1
################################################################

test.schedulerRuleFrequency <- function(biodb) {

	# Delete all connectors
	biodb$getFactory()$deleteAllConnectors()

	# Get scheduler
	scheduler <- biodb$getRequestScheduler()

	# Get ChEBI connector
	chebi <- biodb$getFactory()$getConn('chebi')
	chebi$setSchedulerNParam(3)
	chebi$setSchedulerTParam(1)

	# Get connector rule
	rules <- scheduler$.getConnectorRules(chebi)
	testthat::expect_is(rules, 'list')
	testthat::expect_length(rules, 1)
	rule <- rules[[1]]
	testthat::expect_is(rule, 'BiodbRequestSchedulerRule')
	testthat::expect_equal(rule$getN(), chebi$getSchedulerNParam())
	testthat::expect_equal(rule$getT(), chebi$getSchedulerTParam())

	# Create another ChEBI connector
	chebi.2 <- biodb$getFactory()$createConn('chebi', fail.if.exists = FALSE)
	testthat::expect_length(scheduler$.getConnectorRules(chebi.2), 1)
	testthat::expect_identical(rule, scheduler$.getConnectorRules(chebi.2)[[1]])
	testthat::expect_equal(rule$getN(), chebi$getSchedulerNParam())
	testthat::expect_equal(rule$getT(), chebi$getSchedulerTParam())

	# Change frequency of second connector
	n <- rule$getN()
	chebi.2$setSchedulerNParam(n + 1)
	testthat::expect_equal(rule$getN(), n)
	chebi.2$setSchedulerNParam(n - 1)
	testthat::expect_equal(rule$getN(), n - 1)
	chebi.2$setSchedulerNParam(n)
	testthat::expect_equal(rule$getN(), n)
	t <- rule$getT()
	chebi.2$setSchedulerTParam(t + 0.5)
	testthat::expect_equal(rule$getT(), t + 0.5)
	chebi.2$setSchedulerTParam(t - 0.5)
	testthat::expect_equal(rule$getT(), t)
	chebi.2$setSchedulerTParam(t * 2)
	chebi.2$setSchedulerNParam(n * 2)
	testthat::expect_equal(rule$getN(), n)
	testthat::expect_equal(rule$getT(), t)
}

# Test scheduler sleep time {{{1
################################################################

test.schedulerSleepTime <- function(biodb) {

	n <- 3
	t <- 1.0

	# Delete all connectors
	biodb$getFactory()$deleteAllConnectors()

	# Get scheduler
	scheduler <- biodb$getRequestScheduler()

	# Get ChEBI connector
	chebi <- biodb$getFactory()$getConn('chebi')
	chebi$setSchedulerNParam(n)
	chebi$setSchedulerTParam(t)

	# Get connector rule
	rules <- scheduler$.getConnectorRules(chebi)
	testthat::expect_is(rules, 'list')
	testthat::expect_length(rules, 1)
	rule <- rules[[1]]
	testthat::expect_is(rule, 'BiodbRequestSchedulerRule')

	# Test sleep time
	cur.time <- Sys.time()
	for (i in seq(n)) {
		tt <- cur.time + (i - 1) * t / 10
		testthat::expect_equal(rule$.computeSleepTime(tt), 0)
		rule$.storeCurrentTime(tt)
	}
	testthat::expect_equal(rule$.computeSleepTime(cur.time), t)
	testthat::expect_true(abs(rule$.computeSleepTime(cur.time + t - 0.1) - 0.1) < 1e-6)
	testthat::expect_equal(rule$.computeSleepTime(cur.time + t), 0)
	rule$.storeCurrentTime(cur.time + t)
	testthat::expect_true(abs(rule$.computeSleepTime(cur.time + t) - t / 10) < 1e-6)
}

# Test BiodbUrl {{{1
################################################################

test.BiodbUrl <- function(biodb) {

	# Simple URL
	url <- BiodbUrl(url = 'https://www.somesite.fr')
	testthat::expect_equal(url$toString(), 'https://www.somesite.fr')
	url <- BiodbUrl(url = 'https://www.somesite.fr/')
	testthat::expect_equal(url$toString(), 'https://www.somesite.fr')
	url <- BiodbUrl(url = c('https://www.somesite.fr', ''))
	testthat::expect_equal(url$toString(), 'https://www.somesite.fr/')

	# URL in multiple parts
	url <- BiodbUrl(url = c('https://www.somesite.fr/', 'some', 'page'))
	testthat::expect_equal(url$toString(), 'https://www.somesite.fr/some/page')
	url <- BiodbUrl(url = c('https://www.somesite.fr//', 'some', '/page/'))
	testthat::expect_equal(url$toString(), 'https://www.somesite.fr/some/page')

	# With an unnamed parameter in a character vector
	url <- BiodbUrl(url = 'https://www.somesite.fr/somepage', params = c('rerun'))
	testthat::expect_equal(url$toString(), 'https://www.somesite.fr/somepage?rerun')

	# With a parameter in a character vector
	url <- BiodbUrl(url = 'https://www.somesite.fr/somepage', params = c(format = 'txt'))
	testthat::expect_equal(url$toString(), 'https://www.somesite.fr/somepage?format=txt')

	# With a parameter in a numeric vector
	url <- BiodbUrl(url = 'https://www.somesite.fr/somepage', params = c(limit = 2))
	testthat::expect_equal(url$toString(), 'https://www.somesite.fr/somepage?limit=2')

	# With two parameters in a character vector
	url <- BiodbUrl(url = 'https://www.somesite.fr/somepage', params = c(format = 'txt', limit = '2'))
	testthat::expect_equal(url$toString(), 'https://www.somesite.fr/somepage?format=txt&limit=2')

	# With a parameter in a list
	url <- BiodbUrl(url = 'https://www.somesite.fr/somepage', params = list(format = 'txt'))
	testthat::expect_equal(url$toString(), 'https://www.somesite.fr/somepage?format=txt')

	# With two parameters in a list
	url <- BiodbUrl(url = 'https://www.somesite.fr/somepage', params = list(format = 'txt', limit = 2))
	testthat::expect_equal(url$toString(), 'https://www.somesite.fr/somepage?format=txt&limit=2')
}

# Main {{{1
################################################################

biodb::testThat("BiodbUrl works fine.", test.BiodbUrl, biodb = biodb)
biodb::testThat("Right rule is created.", test.schedulerRightRule, biodb = biodb)
biodb::testThat("Frequency is updated correctly.", test.schedulerRuleFrequency, biodb = biodb)
biodb::testThat("Sleep time is computed correctly.", test.schedulerSleepTime, biodb = biodb)

# Terminate Biodb
biodb$terminate()
