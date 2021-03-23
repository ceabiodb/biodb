test_wsFind <- function(conn) {
    
    # Get request
    testthat::expect_match(conn$wsFind('aaa', retfmt='request'),
                           regexp="^https?://.*$")
    
    # Get plain format
    testthat::expect_match(conn$wsFind('aaa', retfmt='plain'),
                           regexp="^\\{.*\\}$")
    
    # Get parsed format 
    testthat::expect_is(conn$wsFind('aaa', retfmt='parsed'), 'list')
    
    # Get IDs
    testthat::expect_is(conn$wsFind('aaa', retfmt='ids'), 'character')
}

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance(log='test_200_example.log', ack=TRUE)

# Set test context
biodb::setTestContext(biodb, "Example tests")

# Load package definitions
file <- system.file("definitions.yml", package='{{pkgName}}')
biodb$loadDefinitions(file)

# Create connector
conn <- biodb$getFactory()$createConn('{{dbName}}')

# Run tests
biodb::testThat('Find web service is well implemented.',
                test_wsFind, conn=conn)
# Terminate Biodb
biodb$terminate()
