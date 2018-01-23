# vi: fdm=marker

source('common.R')

# Test BiodbCache show {{{1
################################################################

test.BiodbCache.show <- function() {
	biodb <- Biodb$new(logger = FALSE)
	expect_output(biodb$getCache()$show(), regexp = '^Biodb cache .* instance\\..*$')
}

# Test BiodbConfig show {{{1
################################################################

test.BiodbConfig.show <- function() {
	biodb <- Biodb$new(logger = FALSE)
	expect_output(biodb$getConfig()$show(), regexp = '^Biodb config.* instance\\..*Values:.*$')
}

# Test Biodb show {{{1
################################################################

test.Biodb.show <- function() {
	biodb <- Biodb$new(logger = FALSE)
	expect_output(biodb$show(), regexp = '^Biodb instance\\.$')
}

# MAIN {{{1
################################################################

context("Test object information printing")
test_that("Biodb show method returns correct information.", test.Biodb.show())
test_that("BiodbCache show method returns correct information.", test.BiodbCache.show())
test_that("BiodbConfig show method returns correct information.", test.BiodbConfig.show())
