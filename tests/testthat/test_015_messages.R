test.deprecatedMethods <- function(biodb) {

    # Create database and connector
    id <- 'C1'
    db.df <- rbind(data.frame(), list(accession=id, ms.mode='POS',
                                      peak.mztheo=112.07569,
                                      peak.comp='P9Z6W410 O',
                                      peak.attr='[(M+H)-(H2O)-(NH3)]+',
                                      formula="J114L6M62O2",
                                      molecular.mass=146.10553,
                                      name='Blablaine'),
                   stringsAsFactors=FALSE)
    conn <- biodb$getFactory()$createConn('mass.csv.file')
    conn$setDb(db.df)

    # Get entry
    entry <- conn$getEntry(id)

    # Test deprecated methods
    lifecycle::expect_deprecated(conn$getBaseUrl())
    lifecycle::expect_deprecated(conn$setBaseUrl('https://foo/'))
    lifecycle::expect_deprecated(conn$getWsUrl())
    lifecycle::expect_deprecated(conn$setWsUrl('https://foo/'))
    lifecycle::expect_deprecated(conn$getUrl('base.url'))
    lifecycle::expect_deprecated(conn$setUrl('base.url', 'https://foo/'))
    lifecycle::expect_deprecated(conn$searchByName('foo'))
    
    # Not deprecated anymore
    lifecycle::expect_deprecated(entry$getFieldDef('name')$isVector())
    entry$getFieldCardinality('name')

    # Destroy connector
    biodb$getFactory()$deleteConn(conn$getId())
}

test.default.messages <- function(biodb) {

    msg <- "Hello"

    testthat::expect_message(biodb$info(msg), "^Info message: Hello$",
                             perl=TRUE)
    testthat::expect_message(biodb$progressMsg(msg, index=0, total=10,
                                               first=TRUE, laptime=0),
                             "^Info message: Hello.*ETA.*$", perl=TRUE)
    testthat::expect_warning(biodb$warning(msg), "^.* Hello$", perl=TRUE)
    testthat::expect_error(biodb$error(msg), "^.* Hello$", perl=TRUE)
}

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance(log='messages_test.log')

# Set context
biodb::setTestContext(biodb, "Test messages.")

# Run tests
biodb::testThat("Deprecated methods send correct message.",
                test.deprecatedMethods, biodb=biodb)
biodb::testThat("Test what messages are printed by default.",
                test.default.messages, biodb=biodb)

# Terminate Biodb
biodb$terminate()
