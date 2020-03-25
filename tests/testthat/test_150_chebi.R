# vi: fdm=marker ts=4 et cc=80 tw=80

# Test ChEBI encoding issue in XML {{{1
################################################################

test.chebi.encoding.issue.in.xml <- function(conn) {

	entry.ids <- conn$wsGetLiteEntity(search = "2571",
                                      search.category = 'CHEBI ID',
                                      retfmt = 'ids')
	testthat::expect_is(entry.ids, 'character')
	testthat::expect_length(entry.ids, 1)

    # +- sign (U+00b1) appears as <c2><b1> if encoding is not set to UTF-8:
    # "<chebiName>(<c2><b1>)-2-Heptanol</chebiName>" instead of
    # "<chebiName>(±)-2-Heptanol</chebiName>"
	entry <- conn$getEntry('2571')
}

# Test ChEBI wsGetLiteEntity() {{{1
################################################################

test.chebi.wsGetLiteEntity <- function(conn) {

	# Get Id
	id = "2571"
	entry.ids = conn$wsGetLiteEntity(search = id, search.category = 'CHEBI ID',
                                     retfmt = 'ids')
	testthat::expect_is(entry.ids, 'character')
	testthat::expect_length(entry.ids, 1)
	testthat::expect_identical(entry.ids, id)
}

# Test ChEBI convCasToChebi() {{{1
################################################################

test_chebi_convCasToChebi <- function(conn) {
    
    # Values
    cas2chebi1 <- c('51-41-2'='18357', '87605-72-9'='10000')
    cas2chebi2 <- c(as.list(cas2chebi1), list('14215-68-0'=c('40356', '28037')))
    
    # Get ChEBI IDs
    ids <- conn$convCasToChebi(names(cas2chebi1))
    testthat::expect_is(ids, 'character')
    testthat::expect_identical(ids, unname(cas2chebi1))
    
    # Chech NA
    ids <- conn$convCasToChebi(NA_character_)
    testthat::expect_is(ids, 'character')
    testthat::expect_identical(ids, NA_character_)
    ids <- conn$convCasToChebi(c(names(cas2chebi1), NA_character_))
    testthat::expect_identical(ids, c(unname(cas2chebi1), NA_character_))
    
    # Check with multiple ChEBI IDs for one CAS ID
    ids <- conn$convCasToChebi(names(cas2chebi2))
    testthat::expect_is(ids, 'list')
    testthat::expect_identical(ids, unname(cas2chebi2))
}

# Test ChEBI convInchiToChebi() {{{1
################################################################

test_chebi_convInchiToChebi <- function(conn) {

    # Build InChIs
    inchi <- paste0('InChI=1S/C15H24/c1-9(2)11-7-8-15(4)12-6-5-10(3)14(15)13(11)12/h5,9,11',
                '-14H,6-8H2,1-4H3/t11?,12?,13?,14?,15-/m0/s1')
    inchikey <- 'VLXDPFLIRFYIME-MWHZVNNOSA-N'
    testthat::expect_equal(conn$convInchiToChebi(inchi), '10341')
    testthat::expect_equal(conn$convInchiToChebi(inchikey), '10341')
}

# Main {{{1
################################################################################

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance(log='chebi_test.log')

# Set context
biodb::setTestContext(biodb, "Test ChEBI connector.")

# Create connector
conn <- biodb$getFactory()$createConn('chebi')

# Run tests
biodb::runGenericTests(conn)
biodb::testThat('Web service getLiteEntity works fine.',
          test.chebi.wsGetLiteEntity, conn = conn)
biodb::testThat('ChEBI encoding issue in XML is handled.',
          test.chebi.encoding.issue.in.xml, conn = conn)
biodb::testThat('convCasToChebi() works.',
          test_chebi_convCasToChebi, conn = conn)
biodb::testThat('convInchiToChebi() works.',
          test_chebi_convInchiToChebi, conn = conn)

# Terminate Biodb
biodb$terminate()
