# vi: fdm=marker ts=4 et cc=80

# Test NCI CACTUS wsChemicalIdentifierResolver() {{{1
################################################################

test.nci.cactus.wsChemicalIdentifierResolver <- function(conn) {
    
    # Plain
    res <- conn$wsChemicalIdentifierResolver(structid='557795-19-4',
                                             repr='InChIKEY')
    testthat::expect_is(res, 'character')
    testthat::expect_equal(res, 'InChIKey=WINHZLLDWRZWRT-IUQVRHKZNA-N')
    
    # XML parsed
    res <- conn$wsChemicalIdentifierResolver(structid='557795-19-4',
                                             repr='InChIKEY', xml=TRUE,
                                             retfmt='parsed')
    testthat::expect_is(res, 'XMLInternalDocument')
    
    # XML parsed and IDs returned
    res <- conn$wsChemicalIdentifierResolver(structid='557795-19-4',
                                             repr='InChIKEY', xml=TRUE,
                                             retfmt='ids')
    testthat::expect_is(res, 'character')
    testthat::expect_equal(res, 'InChIKey=WINHZLLDWRZWRT-IUQVRHKZNA-N')
}

# Main {{{1
################################################################

test.that('Web service wsChemicalIdentifierResolver works fine.',
          'test.nci.cactus.wsChemicalIdentifierResolver', conn=conn)
