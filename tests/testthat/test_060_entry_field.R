test_entryFields <- function(biodb) {
    
    fields <- biodb$getEntryFields()$get(c('monoisotopic.mass', 'nominal.mass'))
    testthat::expect_type(fields, 'list')
    for (f in fields)
        testthat::expect_s4_class(f, 'BiodbEntryField')
}

test_newAlias <- function(biodb) {

    efs <- biodb$getEntryFields()
    f <- efs$getFieldNames()[[1]]
    ef <- efs$get(f)

    # Add new alias
    aliases <- ef$getAliases()
    newAlias <- 'FooAlias'
    ef$addAlias(newAlias)
    testthat::expect_equal(ef$getAliases(), c(aliases, newAlias))

    # Remove alias
    ef$removeAlias(newAlias)
    testthat::expect_equal(ef$getAliases(), aliases)
}

test_newComputableFrom <- function(biodb) {

    efs <- biodb$getEntryFields()
    f <- efs$getFieldNames()[[1]]
    ef <- efs$get(f)

    # Add new database
    compFrom <- ef$isComputableFrom()
    directive <- list(database='mydb')
    ef$addComputableFrom(directive)
    compFromRef <- if (is.null(compFrom)) list(directive) else c(compFrom, directive)
    testthat::expect_equal(ef$isComputableFrom(), compFromRef)

    # Remove directive
    ef$removeComputableFrom(directive)
    compFromRef <- if (is.null(compFrom)) list() else compFrom
    testthat::expect_equal(ef$isComputableFrom(), compFromRef)
}

test_updateEntryField <- function(biodb) {

    efs <- biodb$getEntryFields()
    ef <- efs$get('molecular.mass')
    compFrom <- ef$isComputableFrom()
    directive <- list(database='mydb')

    # Create new entry
    newEf <- BiodbEntryField(parent=ef$getParent(), name=ef$getName(),
                             computable.from=list(directive))
    ef$updateWithValuesFrom(newEf)
    compFromRef <- if (is.null(compFrom)) list(directive) else c(compFrom, directive)
    testthat::expect_equal(ef$isComputableFrom(), compFromRef)

    # Remove directive
    ef$removeComputableFrom(directive)
    compFromRef <- if (is.null(compFrom)) list() else compFrom
    testthat::expect_equal(ef$isComputableFrom(), compFromRef)
}

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance()

# Set context
biodb::testContext("Test BiodbEntryField.")

# Run tests
biodb::testThat("BiodbEntryFields class works fine.", test_entryFields,
                biodb=biodb)
biodb::testThat("We can add a new alias.", test_newAlias,
                biodb=biodb)
biodb::testThat("We can add new database for ComputableFrom.",
                test_newComputableFrom, biodb=biodb)
biodb::testThat("We can update an entry field.", test_updateEntryField,
                biodb=biodb)

# Terminate Biodb
biodb$terminate()
