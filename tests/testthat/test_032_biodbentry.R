test.getFieldsAsDataframe <- function(biodb) {

    # Create empty entry
    e <- biodb$getFactory()$createNewEntry('mass.csv.file')
    testthat::expect_is(e, "BiodbEntry")

    # Test empty entry
    x <- e$getFieldsAsDataframe()
    testthat::expect_identical(x, data.frame())

    # Test entry with one field with cardinality One
    id <- 'SomeID'
    e$setFieldValue("accession", id)
    x <- e$getFieldsAsDataframe()
    testthat::expect_equal(x, data.frame(accession=id, stringsAsFactors=FALSE))

    # Test entry with one field with cardinality Many
    mz <- c(80, 90, 100)
    mzColl <- paste(mz, collapse=';')
    e$setFieldValue("peak.mztheo", mz)
    x <- e$getFieldsAsDataframe()
    expX <- data.frame(accession=id, peak.mztheo=mzColl, peak.mz=mzColl,
                       stringsAsFactors=FALSE)
    testthat::expect_equal(x, expX)
    x <- e$getFieldsAsDataframe(flatten=FALSE)
    expX <- data.frame(accession=id, peak.mztheo=mz, peak.mz=mz,
                       stringsAsFactors=FALSE)
    testthat::expect_equal(x, expX)
}

test.getFieldValue <- function(biodb) {

    # Create empty entry
    e <- biodb$getFactory()$createNewEntry('mass.csv.file')
    testthat::expect_is(e, "BiodbEntry")

    # Test empty entry
    testthat::expect_identical(e$getFieldValue('accession'), NA_character_)

    # Test entry with one field with cardinality One
    id <- 'SomeID'
    e$setFieldValue("accession", id)
    testthat::expect_identical(e$getFieldValue('accession'), id)

    # Test entry with one field with cardinality Many
    mz <- c(80, 90, 100)
    e$setFieldValue("peak.mztheo", mz)
    testthat::expect_identical(e$getFieldValue('peak.mztheo'), mz)
    testthat::expect_identical(e$getFieldValue('peak.mztheo', limit=2), mz[1:2])
}

# Main

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance(log='biodbentry_test.log')

# Set context
biodb::setTestContext(biodb, "Test BiodbEntry.")

# Run tests
biodb::testThat("getFieldValue() works correctly.", test.getFieldValue,
                biodb=biodb)
biodb::testThat("getFieldsAsDataframe() works correctly.",
                test.getFieldsAsDataframe, biodb=biodb)

# Terminate Biodb
biodb$terminate()
