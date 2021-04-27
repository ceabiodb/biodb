MASSFILEDB.URL <- system.file("extdata", "massbank_extract_full.tsv",
                              package="biodb")

# Set context
biodb::testContext("MassCsvFile long generic tests")

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance()

# Create connector
conn <- biodb$getFactory()$createConn('mass.csv.file', url=MASSFILEDB.URL)

# Make sure we have no residual cache entries from previous tests
biodb$getPersistentCache()$deleteAllFiles(conn$getCacheId(), fail=FALSE)

# Run generic tests
biodb::runGenericTests(conn, short=FALSE, long=TRUE)

# Terminate Biodb
biodb$terminate()
