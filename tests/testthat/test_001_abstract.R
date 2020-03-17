test.abstractClass.declaration <- function(biodb, obs) {
	expect_error(biodb::BiodbConnBase(parent = biodb))
	expect_equal(obs$getLastMsg(), "Class BiodbConnBase is abstract and thus cannot be instantiated.")
}

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance(log='biodb_test.log')
obs <- biodb::addMsgRecObs(biodb)

# Set context
biodb::setTestContext(biodb, "Test abstract declaration.")

# Run tests
biodb::testThat("An abstract class cannot be instantiated.", test.abstractClass.declaration, biodb=biodb, obs=obs)

# Terminate Biodb
biodb$terminate()
