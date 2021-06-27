test.BiodbCache.show <- function(biodb) {
    expect_output(biodb$getPersistentCache()$show(),
                  regexp='^Biodb persistent cache .* instance\\..*$')
}

test.BiodbConfig.show <- function(biodb) {
    expect_output(biodb$getConfig()$show(), regexp = '^Biodb config.* instance\\..*Values:.*$')
}

test.BiodbMain.print <- function(biodb) {
    expect_output(biodb$print(), regexp='^BiodbMain instance, version [0-9]*\\.[0-9]*\\.[0-9]*\\..*$')
}

test.BiodbFactory.print <- function(biodb) {
    expect_output(biodb$getFactory()$print(),
        regexp='^Biodb factory instance\\.$')
}

test.BiodbEntry.show <- function(biodb) {

    # Create database and connector
    id <- 'C1'
    db.df <- rbind(data.frame(), list(accession=id,
                                      ms.mode='POS',
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
    testthat::expect_output(entry$show(),
                            regexp='^Biodb .* entry instance .*\\.$')

    # Destroy connector
    biodb$getFactory()$deleteConn(conn$getId())
}

test.BiodbConn.show <- function(biodb) {

    # Get connection
    chebi.tsv <- system.file("extdata", "chebi_extract.tsv", package='biodb')
    conn <- biodb$getFactory()$createConn('comp.csv.file', url=chebi.tsv)

    # Test printing
    expect_output(conn$show(), regexp="^Compound CSV File instance\\.\n  Class: comp\\.csv\\.file\\.\n  Package:.*\n  Description:.*\n  ID:.*$")
}

test.BiodbDbsInfo.show <- function(biodb) {
    expect_output(biodb$getDbsInfo()$show(), regexp='^Biodb databases information instance\\.\nThe following databases are defined:.*$')
}

test.BiodbEntryFields.show <- function(biodb) {
    expect_output(biodb$getEntryFields()$show(), regexp = '^Biodb entry fields information instance\\.$')
}

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance()

# Set context
biodb::testContext("Test object printing.")

# Run tests
biodb::testThat("BiodbMain print method returns correct information.", test.BiodbMain.print, biodb = biodb)
biodb::testThat("BiodbCache print method returns correct information.", test.BiodbCache.show, biodb = biodb)
biodb::testThat("BiodbConfig print method returns correct information.", test.BiodbConfig.show, biodb = biodb)
biodb::testThat("BiodbFactory print method returns correct information.", test.BiodbFactory.print, biodb = biodb)
biodb::testThat("BiodbEntry print method returns correct information.", test.BiodbEntry.show, biodb = biodb)
biodb::testThat("BiodbConn print method returns correct information.", test.BiodbConn.show, biodb = biodb)
biodb::testThat("BiodbDbsInfo print method returns correct information.", test.BiodbDbsInfo.show, biodb = biodb)
biodb::testThat("BiodbEntryFields print method returns correct information.", test.BiodbEntryFields.show, biodb = biodb)

# Terminate Biodb
biodb$terminate()
