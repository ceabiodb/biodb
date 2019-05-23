# vi: fdm=marker

# Test abstract class declaration {{{1
################################################################

test.abstract.class.declaration <- function(biodb, obs) {
	expect_error(biodb::BiodbConnBase(parent = biodb))
	expect_equal(obs$lastMsg(), "Class BiodbConnBase is abstract and thus cannot be instantiated.")
}

# Main {{{1
################################################################

test.that("An abstract class cannot be instantiated.", 'test.abstract.class.declaration', biodb = biodb, obs = obs)
