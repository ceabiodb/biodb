# vi: fdm=marker ts=4 et cc=80 tw=80

MASS.SQLITE.URL = file.path(getwd(), 'res', 'mass.sqlite.file.sqlite')

# Main {{{1
################################################################

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance()

# Set context
biodb::setTestContext(biodb, "Test Mass SQLite connector.")

# Create connector
conn <- biodb$getFactory()$createConn('mass.sqlite')
conn$setUrl('base.url', MASS.SQLITE.URL)
biodb$getPersistentCache()$deleteAllFiles(conn$getCacheId()) # Make sure we have no residual cache entries from previous tests

# Run tests
biodb::runGenericTests(conn)

# Terminate Biodb
biodb$terminate()
