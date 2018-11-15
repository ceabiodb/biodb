# vi: fdm=marker

# Test right rule {{{1
################################################################

test.schedulerRightRule <- function(biodb, obs) {

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

test.schedulerRuleFrequency <- function(biodb, obs) {

	# Delete all connectors
	biodb$getFactory()$deleteAllConnectors()

	# Get scheduler
	scheduler <- biodb$getRequestScheduler()

	# Get ChEBI connector
	chebi <- biodb$getFactory()$getConn('chebi')
	print('-------------------------------- test.schedulerRuleFrequency 01')
	print(chebi)
	print(chebi$getSchedulerNParam())
	print(chebi$getSchedulerTParam())
	print('-------------------------------- test.schedulerRuleFrequency 02')
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
	print('-------------------------------- 10')
	chebi.2$setSchedulerNParam(n + 1)
	testthat::expect_equal(rule$getN(), n)
	print('-------------------------------- 11')
	chebi.2$setSchedulerNParam(n - 1)
	testthat::expect_equal(rule$getN(), n - 1)
	print('-------------------------------- 12')
	chebi.2$setSchedulerNParam(n)
	testthat::expect_equal(rule$getN(), n)
	print('-------------------------------- 13')
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

# Run scheduler tests {{{1
################################################################

run.scheduler.tests <- function(biodb, obs) {

	set.test.context(biodb, "Test request scheduler")

	# TODO do some tests offline:
	# 2. Test that frequency are updated correctly if another connector is created with different n and t (or same n and t).
	# 3. Test waiting time of scheduler. How? Write a method that returns the time to wait and test that.

	run.test.that.on.biodb.and.obs("Right rule is created.", 'test.schedulerRightRule', biodb, obs)
	run.test.that.on.biodb.and.obs("Frequency is updated correctly.", 'test.schedulerRuleFrequency', biodb, obs)
}
