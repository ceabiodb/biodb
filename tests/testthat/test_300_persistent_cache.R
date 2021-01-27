CHEBI_FILE <- system.file("extdata", "chebi_extract.tsv", package="biodb")

test_cacheFiles <- function(conn) {
    
    # Get some ids
    ids <- conn$getEntryIds(3)
    testthat::expect_is(ids, 'character')
    testthat::expect_length(ids, 3)
    
    # Get cache instance
    cache <- conn$getBiodb()$getPersistentCache()
    
    # Erase all files inside cache folder
    cache$deleteAllFiles(conn$getCacheId(), fail=FALSE)
    
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

test_deleteFilesForWrongCacheId <- function(biodb, obs) {
    
    cache <- biodb$getPersistentCache()
    
    cacheId <- 'foo'
    msg <- paste0("^.*BiodbPersistentCache::deleteAllFiles.*No cache files exist for ",
                  cacheId, ".$")
    testthat::expect_warning(cache$deleteAllFiles(cacheId, fail=TRUE), msg,
                             perl=TRUE)
}

test_filesExistForConn=function(conn) {
    cache <- conn$getBiodb()$getPersistentCache()
    testthat::expect_true(cache$filesExist(conn$getCacheId()))
    cache$deleteAllFiles(conn$getCacheId())
    testthat::expect_false(cache$filesExist(conn$getCacheId()))
}

test_noFilesExist=function(biodb) {
    cache <- biodb$getPersistentCache()
    cacheId <- 'foo'
    testthat::expect_false(cache$filesExist(cacheId))
}

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance(log='persistent_cache_test.log')
obs <- biodb::addMsgRecObs(biodb)

# Set context
biodb::setTestContext(biodb, "Test persistent cache.")

# Enable cache system for local dbs
biodb$getConfig()$set('use.cache.for.local.db', TRUE)

# Create connector
conn <- biodb$getFactory()$createConn('comp.csv.file', url=CHEBI_FILE)

# Run tests
biodb::testThat('We can list cache files.', test_cacheFiles, conn=conn)
biodb::testThat(paste('We got a warning if we try to delete files for a',
                      'cache.id that has no associated folder.'),
                test_deleteFilesForWrongCacheId, biodb=biodb, obs=obs)
biodb::testThat('We detect if cache files exist for a connector',
                test_filesExistForConn, conn=conn)
biodb::testThat('No cache files exist for unknown cache ID.', test_noFilesExist,
                biodb=biodb)

# Terminate Biodb
biodb$terminate()
