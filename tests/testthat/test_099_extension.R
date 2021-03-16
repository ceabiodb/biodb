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
    
    dbName <- 'test.new.ext.foo.db'
    pkgName <- paste0('biodb', biodb:::connNameToClassPrefix(dbName))
    testFile <- paste0('test_', dbName, '.R')

    # Folder of the new package
    pkgDir <- file.path(getwd(), 'output', pkgName)
    if (dir.exists(pkgDir))
        unlink(pkgDir, recursive=TRUE)
    if ( ! dir.exists(dirname(pkgDir)))
        dir.create(dirname(pkgDir))

    # Create a new extension package skeleton
    biodb::ExtPackage$new(pkgDir, dbName='foo.db', makefile=TRUE,
                      rcpp=TRUE)$generate()

    # Check that some files exist
    testthat::expect_true(file.exists(file.path(pkgDir, 'DESCRIPTION')))
#    testthat::expect_true(file.exists(file.path(pkgDir, 'NAMESPACE')))
#    testthat::expect_true(dir.exists(file.path(pkgDir, 'R')))
    testthat::expect_true(file.exists(file.path(pkgDir, 'Makefile')))
    testthat::expect_true(file.exists(file.path(pkgDir, 'LICENSE')))
#    testthat::expect_true(file.exists(file.path(pkgDir, 'README.md')))
#    testthat::expect_true(file.exists(file.path(pkgDir, '.travis.yml')))
#    testthat::expect_true(file.exists(file.path(pkgDir, '.Rbuildignore')))
#    testthat::expect_true(dir.exists(file.path(pkgDir, 'inst')))
#    testthat::expect_true(file.exists(file.path(pkgDir, 'inst',
#                                                'definitions.yml')))
#    testthat::expect_true(dir.exists(file.path(pkgDir, 'tests')))
#    testthat::expect_true(file.exists(file.path(pkgDir, 'tests', 'testthat.R')))
#    testthat::expect_true(dir.exists(file.path(pkgDir, 'tests', 'testthat')))
#    testthat::expect_true(file.exists(file.path(pkgDir, 'tests', 'testthat',
#                                                testFile)))
#    testthat::expect_true(dir.exists(file.path(pkgDir, 'vignettes')))
    
    # Try running tests, generating doc and vignette, etc.
}

test_upgradeExtPkg <- function() {
    
    dbName <- 'test.upgrade.ext.foo.db'
    pkgName <- paste0('biodb', biodb:::connNameToClassPrefix(dbName))
    testFile <- paste0('test_', dbName, '.R')

    # Folder of the new package
    pkgDir <- file.path(getwd(), 'output', pkgName)
    if (dir.exists(pkgDir))
        unlink(pkgDir, recursive=TRUE)
    if ( ! dir.exists(dirname(pkgDir)))
        dir.create(dirname(pkgDir))

    # Create a new extension package skeleton
    biodb::ExtPackage$new(pkgDir)$generate()
    testthat::expect_true(file.exists(file.path(pkgDir, 'DESCRIPTION')))
#    testthat::expect_true(file.exists(file.path(pkgDir, 'NAMESPACE')))
#    testthat::expect_true(dir.exists(file.path(pkgDir, 'R')))
    testthat::expect_true( ! file.exists(file.path(pkgDir, 'Makefile')))
    
    # Upgrade
    biodb::ExtPackage$new(pkgDir, makefile=TRUE)$upgrade()
    testthat::expect_true(file.exists(file.path(pkgDir, 'Makefile')))
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
