# vi: fdm=marker

# Test abstract class declaration {{{1
################################################################

test.abstractClass.declaration <- function(biodb, obs) {
	expect_error(biodb::BiodbConnBase(parent = biodb))
	expect_equal(obs$lastMsg(), "Class BiodbConnBase is abstract and thus cannot be instantiated.")
}

# Main {{{1
################################################################

biodb::testThat("An abstract class cannot be instantiated.", 'test.abstractClass.declaration', biodb = biodb, obs = obs)
