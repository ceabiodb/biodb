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

    # Define new field
    field_def <- list(n_stars=list(description='The ChEBI stars indicator.',
                                   class='integer'))
    ef <- biodb$getEntryFields()
    ef$define(field_def)
    testthat::expect_true(ef$isDefined('n_stars'))

    # Define new parsing expression
    xpathExpr <- '//chebi:return/chebi:entityStar'
    expr_def <- list(chebi=list(parsing.expr=list(n_stars=xpathExpr)))
    di <- biodb$getDbsInfo()
    di$define(expr_def)

    # Check that the expression works
    conn <- biodb$getFactory()$getConn('chebi')
    entry <- conn$getEntry('15440')
    testthat::expect_is(entry, 'BiodbEntry')
    v <- entry$getFieldValue('n_stars')
    testthat::expect_is(v, 'integer')
}

# Main {{{1
################################################################################

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance(log='extension_test.log')
obs <- biodb::addMsgRecObs(biodb)

# Set context
biodb::setTestContext(biodb, "Test definition of extensions.")

# Run tests
biodb::testThat("We can define a new field.", test_new_field, biodb = biodb)
biodb::testThat("We can define a new parsing expression.", test_new_parsing_expr, biodb = biodb)

# Terminate Biodb
biodb$terminate()
