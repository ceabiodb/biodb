# vi: fdm=marker

context("Test abstract declarations")

source('common.R')

# Test abstract class declaration {{{1
################################################################

test.abstract.class.declaration <- function(biodb, obs) {
	expect_error(biodb::BiodbConnBase(parent = biodb))
	expect_equal(obs$lastMsg(), "Class BiodbConnBase is abstract and thus cannot be instantiated.")
}

# MAIN {{{1
################################################################

biodb <- Biodb$new(logger = FALSE)
obs <- create.test.observer(biodb)
set.mode(biodb, MODE.OFFLINE)
test_that("An abstract class cannot be instantiated.", test.abstract.class.declaration(biodb, obs))
