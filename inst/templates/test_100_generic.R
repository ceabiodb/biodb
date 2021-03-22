# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance(log='test_100_generic.log', ack=TRUE)

# Load package definitions
file <- system.file("definitions.yml", package='{{pkgName}}')
biodb$loadDefinitions(file)

# Set test context
biodb::setTestContext(biodb, "Generic tests")

# Create connector
conn <- biodb$getFactory()$createConn('{{dbName}}')

# Run generic tests
biodb::runGenericTests(conn, list(max.results=1))

# Terminate Biodb
biodb$terminate()
