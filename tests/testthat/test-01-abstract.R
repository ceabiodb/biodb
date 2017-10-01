# vi: fdm=marker

source('common.R')

# Test abstract class declaration {{{2
################################################################

test.abstract.class.declaration <- function(biodb, obs) {
	expect_error(biodb::BiodbConn(parent = biodb))
	expect_equal(obs$lastMsg(), "Class BiodbConn is abstract and thus cannot be instantiated.")
}

# MAIN {{{1
################################################################

biodb <- Biodb$new(logger = FALSE)
obs <- create.test.observer(biodb)
set.test.context(biodb, "Test abstract declarations")
set.mode(biodb, MODE.OFFLINE)
test_that("An abstract class cannot be instantiated.", test.abstract.class.declaration(biodb, obs))
