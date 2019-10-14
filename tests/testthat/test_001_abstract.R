# vi: fdm=marker

source('common.R', local=TRUE)
biodb <- biodb::createBiodbTestInstance()
obs <- biodb::addMsgRecObs(biodb)

# Set context
biodb::setTestContext(biodb, "Test abstract declaration.")

# Test abstract class declaration {{{1
################################################################

test.abstractClass.declaration <- function(biodb, obs) {
	expect_error(biodb::BiodbConnBase(parent = biodb))
	expect_equal(obs$lastMsg(), "Class BiodbConnBase is abstract and thus cannot be instantiated.")
}

# Main {{{1
################################################################

biodb::testThat("An abstract class cannot be instantiated.", test.abstractClass.declaration, biodb=biodb, obs=obs)

# Terminate Biodb
biodb$terminate()
