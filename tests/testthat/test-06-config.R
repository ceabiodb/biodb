# vi: fdm=marker

context("Test configuration")

source('common.R')

# Test listKeys {{{1
################################################################

test.listKeys <- function(biodb) {
	keys <- biodb$getConfig()$listKeys()
	expect_is(keys, 'data.frame')
	expect_true(nrow(keys) > 10)
	expect_true(all(vapply(keys, class, FUN.VALUE = '') == 'character'))
	expect_true(all(c('Description') %in% names(keys)))
	expect_false(any(is.na(keys$Description)))
}

# MAIN {{{1
################################################################

biodb <- Biodb$new(logger = FALSE)
set.mode(biodb, MODE.OFFLINE)
test_that("Keys are listed correctly.", test.listKeys(biodb))
