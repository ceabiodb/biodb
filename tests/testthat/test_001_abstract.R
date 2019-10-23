# vi: fdm=marker ts=4 et cc=80 tw=80

# Test abstract class declaration {{{1
################################################################

test.abstractClass.declaration <- function(biodb, obs) {
	expect_error(biodb::BiodbConnBase(parent = biodb))
	expect_equal(obs$getLastMsg(), "Class BiodbConnBase is abstract and thus cannot be instantiated.")
}

# Main {{{1
################################################################

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance()
obs <- biodb::addMsgRecObs(biodb)

# Set context
biodb::setTestContext(biodb, "Test abstract declaration.")

# Run tests
biodb::testThat("An abstract class cannot be instantiated.", test.abstractClass.declaration, biodb=biodb, obs=obs)

# Terminate Biodb
biodb$terminate()
