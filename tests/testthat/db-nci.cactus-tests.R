# vi: fdm=marker ts=4 et cc=80 tw=80

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

    # SMILES to InChI
    res <- conn$wsChemicalIdentifierResolver(structid='C=O',
                                             repr='InChI')
    testthat::expect_is(res, 'character')
    testthat::expect_equal(res, 'InChI=1/CH2O/c1-2/h1H2')
}

# Test convCasToInchi() {{{1
################################################################

test.nci.cactus.convCasToInchi <- function(conn) {
    
    cas2inchi <-
        c('557795-19-4'=paste0(
'InChI=1/C22H27FN4O2/c1-5-27(6-2)10-9-24-22(29)20-13(3)19(25-14(20)4)12-17',
'-16-11-15(23)7-8-18(16)26-21(17)28/h7-8,11-12,25H,5-6,9-10H2,1-4H3,(H,24,2',
'9)(H,26,28)/b17-12-/f/h24,26H'),
          '557795-1-4'=NA_character_,
          '75-07-0'='InChI=1/C2H4O/c1-2-3/h2H,1H3')
 
    inchi <- conn$convCasToInchi(names(cas2inchi))
    testthat::expect_is(inchi, 'character')
    testthat::expect_identical(inchi, unname(cas2inchi))
}

# Test convCasToInchikey() {{{1
################################################################

test.nci.cactus.convCasToInchikey <- function(conn) {
    
    cas2inchikey <- c('557795-19-4'='WINHZLLDWRZWRT-IUQVRHKZNA-N',
                      '75-07-0'='IKHGUXGNUITLKF-UHFFFAOYNA-N',
                      '557795-1-4'=NA_character_)

    inchikey <- conn$convCasToInchikey(names(cas2inchikey))
    testthat::expect_is(inchikey, 'character')
    testthat::expect_identical(inchikey, unname(cas2inchikey))
    
    # Check that passing factors work too
    inchikey <- conn$convCasToInchikey(as.factor(names(cas2inchikey)))
    testthat::expect_is(inchikey, 'character')
    testthat::expect_identical(inchikey, unname(cas2inchikey))
}

# Main {{{1
################################################################

biodb::testThat('Web service wsChemicalIdentifierResolver works fine.',
          test.nci.cactus.wsChemicalIdentifierResolver, conn=conn)
biodb::testThat('convCasToInchi() works fine.',
          test.nci.cactus.convCasToInchi, conn=conn)
biodb::testThat('convCasToInchikey() works fine.',
          test.nci.cactus.convCasToInchikey, conn=conn)
