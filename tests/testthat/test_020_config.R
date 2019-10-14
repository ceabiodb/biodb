# vi: fdm=marker

source('common.R', local=TRUE)
biodb <- biodb::createBiodbTestInstance()

# Set context
biodb::setTestContext(biodb, "Test config.")

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

# Main {{{1
################################################################

biodb::testThat("Keys are listed correctly.", test.listKeys, biodb = biodb)

# Terminate Biodb
biodb$terminate()
