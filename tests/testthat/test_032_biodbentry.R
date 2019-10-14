# vi: fdm=marker

source('common.R', local=TRUE)
biodb <- biodb::createBiodbTestInstance()
obs <- biodb::addMsgRecObs(biodb)

# Set context
biodb::setTestContext(biodb, "Test BiodbEntry.")

# Test getFieldValue() {{{1
################################################################################

test.getFieldValue <- function(biodb, obs) {

    # Create database
    db <- data.frame(
        accession=c("A1", "A1", "A1", "A1", "A1"),
        msprecmz=c(80, 90, 100, 110, 120),
        stringsAsFactors=FALSE)

    # Create connector
    conn <- biodb$getFactory()$createConn('mass.csv.file')
    conn$setDb(db)

    # Get entry
    entry <- conn$getEntry("A1")
    testthat::expect_is(entry, "BiodbEntry")

    # Test
    v <- entry$getFieldValue('msprecmz')
    testthat::expect_identical(v, db$msprecmz)
    v <- entry$getFieldValue('msprecmz', limit=3)
    testthat::expect_identical(v, db$msprecmz[1:3])

    # Delete connector
    biodb$getFactory()$deleteConn(conn$getId())
}

# Main {{{1
################################################################################

biodb::testThat("getFieldValue() works correctly.", test.getFieldValue, biodb = biodb, obs = obs)

# Terminate Biodb
biodb$terminate()
