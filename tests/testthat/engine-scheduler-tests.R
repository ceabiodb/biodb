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

# Run scheduler tests {{{1
################################################################

run.scheduler.tests <- function(biodb, obs) {

	set.test.context(biodb, "Test request scheduler")

	# TODO do some tests offline:
	# 2. Test that frequency are updated correctly if another connector is created with different n and t (or same n and t).
	# 3. Test waiting time of scheduler. How? Write a method that returns the time to wait and test that.

	run.test.that.on.biodb.and.obs("Right rule is created.", 'test.schedulerRightRule', biodb, obs)
}
