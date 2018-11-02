# vi: fdm=marker

# Test abstract class declaration {{{1
################################################################

test.abstract.class.declaration <- function(biodb, obs) {
	expect_error(biodb::BiodbConnBase(parent = biodb))
	expect_equal(obs$lastMsg(), "Class BiodbConnBase is abstract and thus cannot be instantiated.")
}

# Run abstract tests {{{1
################################################################

run.abstract.tests <- function(biodb, obs) {

	set.test.context(biodb, "Test abstract declarations")

	run.test.that.on.biodb.and.obs("An abstract class cannot be instantiated.", 'test.abstract.class.declaration', biodb, obs)
}
