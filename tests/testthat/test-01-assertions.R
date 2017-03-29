# vi: fdm=marker

source('common.R')

# Create test observer {{{1
################################################################

create.test.observer <- function(biodb) {

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

	return(obs)
}

# Test assert.positive {{{1
################################################################

test.assert.positive <- function(biodb, obs) {

	for (mz in c(10, 0))
		biodb$.assert.positive(mz)
	mz <- -1
	expect_error(biodb$.assert.positive(mz))
	expect_equal(obs$lastMsg(), "mz (-1) cannot be negative.")
}

# Test assert.in {{{1
################################################################

test.assert.in <- function(biodb, obs) {

	biodb$.assert.in('aaa', c('aaa', 'bbb'))
	str <- 'ccc'
	expect_error(biodb$.assert.in(str, c('aaa', 'bbb')))
	expect_equal(obs$lastMsg(), "str cannot be set to ccc. Allowed values are: aaa, bbb.")
}

# Test assert.not.na {{{1
################################################################

test.assert.not.na <- function(biodb, obs) {

	biodb$.assert.not.na(10)
	biodb$.assert.not.na(c(1, 3, 10))
	for (myvar in list(NA_real_, c(1.0, NA_real_))) {
		expect_error(biodb$.assert.not.na(myvar))
		expect_equal(obs$lastMsg(), "myvar cannot be set to NA.")
	}
}

# Test assert.not.null {{{1
################################################################

test.assert.not.null <- function(biodb, obs) {

	biodb$.assert.not.null(10)
	myvar <- NULL
	expect_error(biodb$.assert.not.null(myvar))
	expect_equal(obs$lastMsg(), "myvar cannot be NULL.")
}

# MAIN {{{1
################################################################

biodb <- Biodb$new(logger = FALSE)
obs <- create.test.observer(biodb)
set.test.context(biodb, "Test assertions")
set.mode(biodb, MODE.OFFLINE)
test_that("Assertion of positive number works correctly", test.assert.positive(biodb, obs))
test_that("Assertion of enumerate works correctly", test.assert.in(biodb, obs))
test_that("Assertion of non NA value works correctly", test.assert.not.na(biodb, obs))
test_that("Assertion of non NULL value works correctly", test.assert.not.null(biodb, obs))
