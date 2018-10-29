# vi: fdm=marker

context("Test factory")

source('common.R')

# Test connector already exists {{{1
################################################################

test.connectorAlreadyExists <- function(biodb, obs) {

	# Test with Mass CSV file
	conn <- biodb$getFactory()$createConn('mass.csv.file', url = MASSFILEDB.URL)
	testthat::expect_error(biodb$getFactory()$createConn('mass.csv.file', url = MASSFILEDB.URL))
	testthat::expect_error(biodb$getFactory()$createConn('mass.csv.file', url = file.path('..', basename(getwd()), MASSFILEDB.URL)))
	expect_equal(obs$lastMsg(), "blabla")
	conn.2 <- biodb$getFactory()$createConn('mass.csv.file', url = file.path('..', basename(getwd()), MASSFILEDB.URL), fail.if.exists = FALSE)
	expect_equal(obs$lastMsg(), "blabla")

	# Test with ChEBI
	conn.chebi <- biodb$getFactory()$createConn('chebi')
	testthat::expect_error(biodb$getFactory()$createConn('chebi'))
	expect_equal(obs$lastMsg(), "blabla")
	conn.chebi.2 <- biodb$getFactory()$createConn('chebi', fail.if.exists = FALSE)
	expect_equal(obs$lastMsg(), "blabla")
}

# MAIN {{{1
################################################################

biodb <- Biodb$new(logger = FALSE)
obs <- create.test.observer(biodb)
set.mode(biodb, MODE.OFFLINE)
test_that("We detect when an identical connector already exists.", test.connectorAlreadyExists(biodb, obs))
biodb$terminate()
