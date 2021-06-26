# vi: fdm=marker ts=4 et cc=80 tw=80

# Test named properties {{{1
################################################################################

test.namedProp <- function(biodb) {
    prop <- list(name='MyDb', entry.content.type='txt')
    dbinfo <- biodb::BiodbDbInfo(db.class='mydb',
                                 cfg=biodb$getConfig(),
                                 properties=prop)
    url <- 'http://some/url/that/points/somewhere'
    dbinfo$setPropValSlot('urls', 'myurl', url)
    url2 <- dbinfo$getPropValSlot('urls', 'myurl')
    testthat::expect_equal(url, url2)
}

# Main {{{1
################################################################################

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance()

# Set context
biodb::testContext("Test BiodbDbInfo.")

# Run tests
biodb::testThat("Named properties work correctly.", test.namedProp, biodb=biodb)

# Terminate Biodb
biodb$terminate()
