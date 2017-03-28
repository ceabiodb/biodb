# vi: fdm=marker

source('common.R')

# Test assertions {{{1
################################################################

test.assertions <- function(biodb) {

	# Create test observer class
	TestObs <- methods::setRefClass("TestObs", contains = "BiodbObserver", fields = list(msgs = 'character'))
	TestObs$methods( initialize = function(...) {
		msgs <<- character(0)
	})
	TestObs$methods( message = function(type, msg, class = NA_character_, method = NA_character_, level = 1) {
		msgs <<- c(.self$msgs, msg)
	})
	TestObs$methods( lastMsg = function() {
		return(.self$msgs[[length(.self$msgs)]])
	})
	obs <- TestObs$new()

	# Set observer
	biodb$addObservers(obs)

	# Test .assert.positive
	for (mz in c(10, 0))
		biodb$.assert.positive(mz)
	mz <- -1
	expect_error(biodb$.assert.positive(mz))
	expect_equal(obs$lastMsg(), "mz (-1) cannot be negative.")

	# Test .assert.in
	biodb$.assert.in('aaa', c('aaa', 'bbb'))
	str <- 'ccc'
	expect_error(biodb$.assert.in(str, c('aaa', 'bbb')))
	expect_equal(obs$lastMsg(), "str cannot be set to ccc. Allowed values are: aaa, bbb.")
}

# MAIN {{{1
################################################################

biodb <- Biodb$new(logger = FALSE)
set.test.context(biodb, "Test assertions")
set.mode(biodb, MODE.OFFLINE)
test_that("Assertions work correctly", test.assertions(biodb))
