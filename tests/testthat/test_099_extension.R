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

test_newExtPkgSkeleton <- function() {
    
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
        
        # Check targets
        system(paste0('make -C "', pkgDir, '" doc'))
        testthat::expect_true(file.exists(file.path(pkgDir, 'NAMESPACE')))
        system(paste0('make -C "', pkgDir, '"')) # run compilation if any
        system(paste0('make -C "', pkgDir, '" test'))

        # Vignette cannot be built
#        system(paste0('make -C "', pkgDir, '" check'))
    }
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
        biodb::ExtPackage$new(path=pkgDir, dbName=dbName, dbTitle='FOO
                              database', connType=cfg$connType,
                              entryType=cfg$entryType, remote=cfg$remote,
                              downloadable=downloadable,
                              editable=!cfg$remote, writable=!cfg$remote,
                              makefile=FALSE, rcpp=FALSE)$generate()
        
        # Test files
        testthat::expect_true(file.exists(file.path(pkgDir, 'DESCRIPTION')))
        testthat::expect_false(file.exists(file.path(pkgDir, 'Makefile')))
        testthat::expect_false(dir.exists(file.path(pkgDir, 'src')))

        # Upgrade
        biodb::ExtPackage$new(path=pkgDir, dbName=dbName, dbTitle='FOO
                              database', connType=cfg$connType,
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

# Main
################################################################################

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance(log='extension_test.log')
obs <- biodb::addMsgRecObs(biodb)

# Set context
biodb::setTestContext(biodb, "Test definition of extensions.")

# Run tests
biodb::testThat("We can define a new field.", test_new_field, biodb=biodb)
biodb::testThat("We can define a new parsing expression.",
                test_new_parsing_expr, biodb=biodb)
biodb::testThat("show() method works correctly.", test_chebiExShow,
                biodb=biodb)
biodb::testThat("We can generate a skeleton for a new extension package.",
                test_newExtPkgSkeleton)
biodb::testThat("We can upgrade the files of an extension package.",
                test_upgradeExtPkg)

# Terminate Biodb
biodb$terminate()
