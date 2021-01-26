CHEBI_FILE <- system.file("extdata", "chebi_extract.tsv", package="biodb")

test_cacheFiles <- function(conn) {
    
    # Get some ids
    ids <- conn$getEntryIds(3)
    testthat::expect_is(ids, 'character')
    testthat::expect_length(ids, 3)
    
    # Get cache instance
    cache <- conn$getBiodb()$getPersistentCache()
    
    # Erase all files inside cache folder
    cache$deleteAllFiles(conn$getCacheId())
    
    # Get extension
    ctype <- conn$getPropertyValue('entry.content.type')
    
    # Get entries
    entries <- conn$getEntry(ids)

    # Get cache file paths
    files <- cache$listFiles(conn$getCacheId(), ext=ctype, full.path=TRUE)
    testthat::expect_is(files, 'character')
    testthat::expect_length(files, 3)
    testthat::expect_true(all(vapply(files, file.exists, FUN.VALUE=TRUE)))
        
    # Get back IDs from cache file names
    idsFromCache <- cache$listFiles(conn$getCacheId(), ext=ctype,
                                    extract.name=TRUE)
    testthat::expect_is(idsFromCache, 'character')
    testthat::expect_length(idsFromCache, 3)
    testthat::expect_equal(ids, idsFromCache)
}

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance(log='persistent_cache_test.log')

# Set context
biodb::setTestContext(biodb, "Test persistent cache.")

# Enable cache system for local dbs
biodb$getConfig()$set('use.cache.for.local.db', TRUE)

# Create connector
conn <- biodb$getFactory()$createConn('comp.csv.file', url=CHEBI_FILE)

# Run tests
biodb::testThat('We can list cache files.', test_cacheFiles, conn=conn)

# Terminate Biodb
biodb$terminate()
