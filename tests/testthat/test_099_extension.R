test_new_field <- function(biodb) {

    field_def <- list(n_stars=list(description='The ChEBI stars indicator.',
                                   class='integer'))
    ef <- biodb$getEntryFields()
    ef$define(field_def)
    testthat::expect_true(ef$isDefined('n_stars'))
}

test_new_parsing_expr <- function(biodb) {

    # Load ChEBI connector definition
    defFile <- system.file("extdata", "chebi_ex.yml", package="biodb")
    connFile <- system.file("extdata", "ChebiExConn.R", package="biodb")
    entryFile <- system.file("extdata", "ChebiExEntry.R", package="biodb")
    biodb$loadDefinitions(defFile)
    source(connFile)
    source(entryFile)

    # Define new field
    field_def <- list(n_stars=list(description='The ChEBI stars indicator.',
                                   class='integer'))
    ef <- biodb$getEntryFields()
    ef$define(field_def)
    testthat::expect_true(ef$isDefined('n_stars'))

    # Define new parsing expression
    xpathExpr <- '//chebi:return/chebi:entityStar'
    expr_def <- list(chebi.ex=list(parsing.expr=list(n_stars=xpathExpr)))
    di <- biodb$getDbsInfo()
    di$define(expr_def)

    # Check that the expression works
    conn <- biodb$getFactory()$getConn('chebi.ex')
    entry <- conn$getEntry('15440')
    testthat::expect_is(entry, 'BiodbEntry')
    v <- entry$getFieldValue('n_stars')
    testthat::expect_is(v, 'integer')
}

test_chebiExShow <- function(biodb) {

    # Load ChEBI connector definition
    defFile <- system.file("extdata", "chebi_ex.yml", package="biodb")
    connFile <- system.file("extdata", "ChebiExConn.R", package="biodb")
    entryFile <- system.file("extdata", "ChebiExEntry.R", package="biodb")
    biodb$loadDefinitions(defFile)
    source(connFile)
    source(entryFile)
    
    # Create connector
    conn <- biodb$getFactory()$getConn('chebi.ex')
    testthat::expect_is(conn, 'ChebiExConn')
    
    # Check scheduler parameters
    testthat::expect_output(print(conn), '^.*Request maximum rate:.*$')
}

test_newExtPkgSkeletonGeneration <- function() {
    
    for (cfg in list(list(connType='plain', remote=TRUE, entryType='plain',
                          rcpp=FALSE)
                     ,list(connType='compound', remote=FALSE, entryType='csv')
                     ,list(connType='compound', remote=FALSE, entryType='list')
                     ,list(connType='mass', remote=FALSE, entryType='csv')
                     ,list(connType='mass', remote=TRUE, entryType='json')
                     ,list(connType='compound', remote=TRUE, entryType='html')
                     ,list(connType='compound', remote=TRUE, entryType='xml',
                           rcpp=TRUE)
                     ,list(connType='compound', remote=TRUE, entryType='xml',
                           downloadable=TRUE)
                     ,list(connType='compound', remote=TRUE, entryType='sdf')
                     ,list(connType='compound', remote=TRUE, entryType='txt')
                     )) {
            
        downloadable <- cfg$remote && (if ('downloadable' %in% names(cfg))
                                       cfg$downloadable else FALSE)
        rcpp <- if ('rcpp' %in% names(cfg))
                                       cfg$rcpp else FALSE
        name <- c('foo', (if (cfg$remote) 'remote' else 'local'))
        if (downloadable)
            name <- c(name, 'dwnld')
        if (rcpp)
            name <- c(name, 'rcpp')
        name <- c(name, cfg$connType, cfg$entryType, 'db')
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
                              connType=cfg$connType, entryType=cfg$entryType,
                              remote=cfg$remote, downloadable=downloadable,
                              editable=!cfg$remote, writable=!cfg$remote,
                              makefile=TRUE, rcpp=rcpp)$generate()

        # Check files & dirs
        testthat::expect_true(file.exists(file.path(pkgDir, 'DESCRIPTION')))
        testthat::expect_true(dir.exists(file.path(pkgDir, 'R')))
        testthat::expect_true(file.exists(file.path(pkgDir, 'R',
                                                    paste0(clsPrefix,
                                                           'Conn.R'))))
        testthat::expect_true(file.exists(file.path(pkgDir, 'R',
                                                    paste0(clsPrefix,
                                                           'Entry.R'))))
        testthat::expect_true(file.exists(file.path(pkgDir, 'R',
                                                    'package.R')))
        testthat::expect_true(file.exists(file.path(pkgDir, 'Makefile')))
        testthat::expect_true(file.exists(file.path(pkgDir, 'LICENSE')))
        testthat::expect_true(file.exists(file.path(pkgDir, 'README.md')))
        testthat::expect_true(file.exists(file.path(pkgDir, '.travis.yml')))
        testthat::expect_true(file.exists(file.path(pkgDir, '.Rbuildignore')))
        testthat::expect_true(file.exists(file.path(pkgDir, '.gitignore')))
        testthat::expect_equal(rcpp, dir.exists(file.path(pkgDir, 'src')))
        testthat::expect_true(dir.exists(file.path(pkgDir, 'inst')))
        testthat::expect_true(file.exists(file.path(pkgDir, 'inst',
                                                    'definitions.yml')))
        testthat::expect_true(dir.exists(file.path(pkgDir, 'tests')))
        testthat::expect_true(file.exists(file.path(pkgDir, 'tests',
                                                    'testthat.R')))
        testthat::expect_true(dir.exists(file.path(pkgDir, 'tests',
                                                   'testthat')))
        testthat::expect_true(file.exists(file.path(pkgDir, 'tests', 'testthat',
                                                    'test_100_generic.R')))
        testthat::expect_true(dir.exists(file.path(pkgDir, 'tests',
                                                   'testthat', 'res')))
        testthat::expect_true(file.exists(file.path(pkgDir, 'tests',
            'testthat', 'res', paste0('entry-', dbName, '-0001.json'))))
        testthat::expect_true(file.exists(file.path(pkgDir, 'tests', 'testthat',
                                                    'test_200_example.R')))
        testthat::expect_true(dir.exists(file.path(pkgDir, 'vignettes')))
    }
}

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
    makeLog <- paste(pkgDir, 'make', 'doc', sep='_')
    system2('make', c('-C', pkgDir, 'doc'), stdout=makeLog, stderr=makeLog)
    testthat::expect_true(file.exists(file.path(pkgDir, 'NAMESPACE')))
    makeLog <- paste(pkgDir, 'make', sep='_')
    system2('make', c('-C', pkgDir), stdout=makeLog, stderr=makeLog)
    makeLog <- paste(pkgDir, 'make', 'test', sep='_')
    system2('make', c('-C', pkgDir, 'test'), stdout=makeLog, stderr=makeLog)
}

test_upgradeExtPkg <- function() {
    
    for (cfg in list(list(connType='plain', remote=TRUE, entryType='plain',
                          rcpp=FALSE)
                     ,list(connType='compound', remote=TRUE, entryType='xml',
                           rcpp=TRUE)
                     ,list(connType='compound', remote=TRUE, entryType='txt')
                     )) {
        downloadable <- cfg$remote && (if ('downloadable' %in% names(cfg))
                                       cfg$downloadable else FALSE)
        rcpp <- cfg$remote && (if ('rcpp' %in% names(cfg))
                                       cfg$rcpp else FALSE)
        name <- c('foo', (if (cfg$remote) 'remote' else 'local'))
        if (downloadable)
            name <- c(name, 'dwnld')
        if (rcpp)
            name <- c(name, 'rcpp')
        name <- c(name, cfg$connType, cfg$entryType, 'db')
        dbName <- paste(name, collapse='.')
        clsPrefix <- biodb:::connNameToClassPrefix(dbName)
        pkgName <- paste0('biodb', clsPrefix)

        # Folder of the new package
        pkgDir <- file.path(getwd(), 'output', pkgName)
        if (dir.exists(pkgDir))
            unlink(pkgDir, recursive=TRUE)
        if ( ! dir.exists(dirname(pkgDir)))
            dir.create(dirname(pkgDir))

        # Create a new extension package
        biodb::ExtPackage$new(path=pkgDir, dbName=dbName,
                              dbTitle='FOO database', connType=cfg$connType,
                              entryType=cfg$entryType, remote=cfg$remote,
                              downloadable=downloadable,
                              editable=!cfg$remote, writable=!cfg$remote,
                              makefile=FALSE, rcpp=FALSE)$generate()
        
        # Test files
        testthat::expect_true(file.exists(file.path(pkgDir, 'DESCRIPTION')))
        testthat::expect_false(file.exists(file.path(pkgDir, 'Makefile')))
        testthat::expect_false(dir.exists(file.path(pkgDir, 'src')))

        # Upgrade
        biodb::ExtPackage$new(path=pkgDir, dbName=dbName,
                              dbTitle='FOO database', connType=cfg$connType,
                              entryType=cfg$entryType, remote=cfg$remote,
                              downloadable=downloadable,
                              editable=!cfg$remote, writable=!cfg$remote,
                              makefile=TRUE, rcpp=rcpp)$upgrade()
        
        # Test files
        testthat::expect_true(file.exists(file.path(pkgDir, 'DESCRIPTION')))
        testthat::expect_true(file.exists(file.path(pkgDir, 'Makefile')))
        if (rcpp)
            testthat::expect_true(dir.exists(file.path(pkgDir, 'src')))
        else
            testthat::expect_false(dir.exists(file.path(pkgDir, 'src')))
    }
}

test_useDotForCurrentDir <- function() {
    
    pkgName <- 'biodbFoo'

    # Folder of the new package
    pkgDir <- file.path(getwd(), 'output', pkgName)
    if (dir.exists(pkgDir))
        unlink(pkgDir, recursive=TRUE)
    if ( ! dir.exists(pkgDir)) # Create empty package folder
        dir.create(pkgDir)

    # Init git repos with fake remote
    git2r::init(pkgDir)
    git2r::remote_add(pkgDir, 'origin',
                      paste0('https://github.com/pkrog/', pkgName, '.git'))

    # Change current path
    curdir <- getwd()
    setwd(pkgDir)

    # Create a new extension package
    biodb::ExtPackage$new(path='.', dbName='foo', dbTitle='FOO database'
                          )$generate()

    # Go back to working dir
    setwd(curdir)

    # Test files
    testthat::expect_true(file.exists(file.path(pkgDir, 'DESCRIPTION')))
}

test_getReposName <- function() {

    # Create local repos folder
    repos <- 'myReposName01'
    localRepos <- file.path(getwd(), 'output', repos)
    if (dir.exists(localRepos))
        unlink(localRepos, recursive=TRUE)
    if ( ! dir.exists(localRepos))
        dir.create(localRepos)

    # Init local repos and set remote URL
    git2r::init(localRepos)
    git2r::remote_add(localRepos, 'origin',
                      paste0('https://github.com/pkrog/', repos, '.git'))
    
    name <- getReposName(localRepos)
    testthat::expect_equal(paste0('pkrog/', repos), name)
}

# Main
################################################################################

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance()

# Set context
biodb::testContext("Test definition of extensions.")

# Run tests
biodb::testThat("We can get the remote repository name.", test_getReposName)
biodb::testThat("We can define a new field.", test_new_field, biodb=biodb)
biodb::testThat("We can define a new parsing expression.",
                test_new_parsing_expr, biodb=biodb)
biodb::testThat("show() method works correctly.", test_chebiExShow,
                biodb=biodb)
biodb::testThat("We can generate a skeleton for a new extension package.",
                test_newExtPkgSkeletonGeneration)
biodb::testThat("We can run make on a newly generated extension package.",
                test_runningMakeOnNewExtPkg)
biodb::testThat("We can upgrade the files of an extension package.",
                test_upgradeExtPkg)
biodb::testThat("We can use '.' (current directory) for the package path.",
                test_useDotForCurrentDir)

# Terminate Biodb
biodb$terminate()
