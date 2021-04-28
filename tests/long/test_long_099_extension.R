test_runningMakeOnNewExtPkg <- function() {
    
    # We run make on just one pkg, since it is quite long

    connType <- 'compound'
    entryType <- 'xml'
    name <- c('foo2', 'local', 'rcpp')
    dbName <- paste(name, collapse='.')
    clsPrefix <- biodb:::connNameToClassPrefix(dbName)
    pkgName <- paste0('biodb', clsPrefix)

    # Folder of the new package
    pkgDir <- file.path(getwd(), 'output', pkgName)
    if (dir.exists(pkgDir))
        unlink(pkgDir, recursive=TRUE)
    if ( ! dir.exists(dirname(pkgDir)))
        dir.create(dirname(pkgDir))

    # Create a new extension package skeleton
    biodb::ExtPackage$new(path=pkgDir, dbName=dbName,
                          dbTitle='FOO database',
                          connType=connType, entryType=entryType,
                          remote=FALSE,
                          editable=TRUE, writable=TRUE,
                          makefile=TRUE, rcpp=TRUE)$generate()
    
    # Check targets
    biodb::logDebug('Package directory is "%s".', pkgDir)
    makeLog <- paste(pkgDir, 'make', 'doc', 'log', sep='.')
    biodb::logDebug('Make log file is "%s".', makeLog)
    system2('make', c('-C', pkgDir, 'doc'), stdout=makeLog, stderr=makeLog)
    testthat::expect_true(file.exists(file.path(pkgDir, 'NAMESPACE')),
                                      info=paste0('Running make doc inside "',
                                                  pkgDir,
                                                  '" failed. See output in "',
                                                  makeLog, '".'))
    makeLog <- paste(pkgDir, 'make', 'log', sep='.')
    biodb::logDebug('Make log file is "%s".', makeLog)
    system2('make', c('-C', pkgDir), stdout=makeLog, stderr=makeLog)
    makeLog <- paste(pkgDir, 'make', 'test', 'log', sep='.')
    biodb::logDebug('Make log file is "%s".', makeLog)
    system2('make', c('-C', pkgDir, 'test'), stdout=makeLog, stderr=makeLog)
}

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance()

# Set context
biodb::testContext("Long tests on extension generator.")

biodb::testThat("We can run make on a newly generated extension package.",
                test_runningMakeOnNewExtPkg)
