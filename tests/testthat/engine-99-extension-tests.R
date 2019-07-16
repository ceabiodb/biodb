# vi: fdm=marker ts=4 et cc=80 tw=80

# Test new field {{{1
################################################################################

test_new_field <- function(biodb) {
    
    field_def <- list(formula2 = list(description = 'New formula field.'))
    ef <- biodb$getEntryFields()
    ef$define(field_def)
    testthat::expect_true(ef$isDefined('formula2'))
}

# Test new parsing expression {{{1
################################################################################

test_new_parsing_expr <- function(biodb) {
    
    # Define new field
    field_def <- list(formula3 = list(description = 'New formula field.'))
    ef <- biodb$getEntryFields()
    ef$define(field_def)
    testthat::expect_true(ef$isDefined('formula3'))
    
    # Define new parsing expression
    expr_def <- list(lipidmaps.structure =
                     list(parsing.expr = list(formula3 = 'FORMULA')))
    di <- biodb$getDbsInfo()
    di$define(expr_def)
    
    # Check that the expression works
    conn <- biodb$getFactory()$getConn('lipidmaps.structure')
    entry <- conn$getEntry('LMFA00000001')
    testthat::expect_is(entry, 'BiodbEntry')
    v <- entry$getFieldValue('formula')
    v3 <- entry$getFieldValue('formula3')
    testthat::expect_equal(v, v3)
}

# Main {{{1
################################################################################

test.that("We can define a new field.", "test_new_field", biodb = biodb)
test.that("We can define a new parsing expression.", "test_new_parsing_expr", biodb = biodb)
