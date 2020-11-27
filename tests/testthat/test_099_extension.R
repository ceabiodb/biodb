# vi: fdm=marker ts=4 et cc=80 tw=80

# Test new field {{{1
################################################################################

test_new_field <- function(biodb) {

    field_def <- list(n_stars=list(description='The ChEBI stars indicator.',
                                   class='integer'))
    ef <- biodb$getEntryFields()
    ef$define(field_def)
    testthat::expect_true(ef$isDefined('n_stars'))
}

# Test new parsing expression {{{1
################################################################################

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

# Main {{{1
################################################################################

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance(log='extension_test.log')
obs <- biodb::addMsgRecObs(biodb)

# Set context
biodb::setTestContext(biodb, "Test definition of extensions.")

# Run tests
biodb::testThat("We can define a new field.", test_new_field, biodb=biodb)
biodb::testThat("We can define a new parsing expression.", test_new_parsing_expr, biodb=biodb)
biodb::testThat("show() method works correctly", test_chebiExShow, biodb=biodb)

# Terminate Biodb
biodb$terminate()
