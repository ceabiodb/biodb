# $$$ SECTION REMOTE $$$
test_wsFind <- function(conn) {
    
    # Get request
    testthat::expect_is(conn$wsFind('aaa', retfmt='request'), 'BiodbRequest')
    
    # Get plain format
    testthat::expect_match(conn$wsFind('aaa', retfmt='plain'),
                           regexp="^\\{.*\\}$")
    
    # Get parsed format 
    testthat::expect_is(conn$wsFind('aaa', retfmt='parsed'), 'list')
    
    # Get IDs
    testthat::expect_is(conn$wsFind('aaa', retfmt='ids'), 'character')
}
# $$$ END_SECTION REMOTE $$$

# Set test context
biodb::testContext("Example long tests")

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance(ack=TRUE)

# Load package definitions
defFile <- system.file("definitions.yml", package='{{pkgName}}')
biodb$loadDefinitions(defFile)

# Create connector
conn <- biodb$getFactory()$createConn('{{dbName}}')

# Run tests
# $$$ SECTION REMOTE $$$
biodb::testThat('Find web service is well implemented.',
                test_wsFind, conn=conn)
# $$$ END_SECTION REMOTE $$$
# TODO Implement your own tests

# Terminate Biodb
biodb$terminate()
