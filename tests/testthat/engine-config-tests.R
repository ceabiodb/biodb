# vi: fdm=marker

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

# Run config tests {{{1
################################################################

run.config.tests <- function(biodb) {

	set.test.context(biodb, "Test configuration")

	run.test.that.on.biodb("Keys are listed correctly.", 'test.listKeys', biodb)
}
