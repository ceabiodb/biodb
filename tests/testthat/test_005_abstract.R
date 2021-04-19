test.abstractClass.declaration <- function(biodb) {
    expect_error(biodb::BiodbConnBase(parent=biodb),
        regexp="Class BiodbConnBase is abstract.*cannot be instantiated.")
}

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance()

# Set context
biodb::testContext("Test abstract declaration.")

# Run tests
biodb::testThat("An abstract class cannot be instantiated.",
                test.abstractClass.declaration, biodb=biodb)

# Terminate Biodb
biodb$terminate()
