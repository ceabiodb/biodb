# vi: fdm=marker

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

# Test assert.inferior {{{1
################################################################

test.assert.inferior <- function(biodb, obs) {

	small <- 10
	big <- 20
	biodb$.assert.inferior(small, big)
	myvar <- NULL
	expect_error(biodb$.assert.inferior(big, small))
	expect_equal(obs$lastMsg(), "big (20) cannot be greater than small (10).")
}

# Test assert.length.one {{{1
################################################################

test.assert.length.one <- function(biodb, obs) {

	biodb$.assert.length.one(10)
	myvar <- c(10, 20)
	expect_error(biodb$.assert.length.one(myvar))
	expect_equal(obs$lastMsg(), "Length of myvar (2) must be one.")
}

# Test searchMsEntries assert {{{1
################################################################

test.searchMsEntries.assert <- function(biodb, obs) {

	conn <- get.default.db(biodb, 'massbank')

	expect_error(ids <- conn$searchMsEntries(mz = 10, mz.tol = 0.01, mz.tol.unit = 'plain', max.results = 1, ms.level = -1))
	expect_equal(obs$lastMsg(), "ms.level (-1) cannot be negative.")
}

# Run assertions tests {{{1
################################################################

run.assertions.tests <- function(biodb, obs) {

	set.test.context(biodb, "Test assertions")

	test.that("Assertion of positive number works correctly", 'test.assert.positive', biodb = biodb, obs = obs)
	test.that("Assertion of enumerate works correctly", 'test.assert.in', biodb = biodb, obs = obs)
	test.that("Assertion of non NA value works correctly", 'test.assert.not.na', biodb = biodb, obs = obs)
	test.that("Assertion of non NULL value works correctly", 'test.assert.not.null', biodb = biodb, obs = obs)
	test.that("Assertion of inferior relationship works correctly", 'test.assert.inferior', biodb = biodb, obs = obs)
	test.that("Assertion of a single element works correctly", 'test.assert.length.one', biodb = biodb, obs = obs)
	test.that('Assertion called from searchMsEntries display the right variable name', 'test.searchMsEntries.assert', biodb = biodb, obs = obs)
}
