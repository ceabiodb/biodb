test.BiodbCache.print <- function(biodb) {
    expect_output(biodb$getPersistentCache()$print(),
                  regexp='^Biodb persistent cache .* instance\\..*$')
}

test.BiodbConfig.print <- function(biodb) {
    expect_output(biodb$getConfig()$print(), regexp = '^Biodb config.* instance\\..*Values:.*$')
}

test.BiodbMain.print <- function(biodb) {
    expect_output(biodb$print(), regexp='^BiodbMain instance, version [0-9]*\\.[0-9]*\\.[0-9]*\\..*$')
}

test.BiodbFactory.print <- function(biodb) {
    expect_output(biodb$getFactory()$print(),
        regexp='^Biodb factory instance\\.$')
}

test.BiodbEntry.print <- function(biodb) {

    # Create database and connector
    id <- 'C1'
    db.df <- rbind(data.frame(), list(accession=id,
        ms.mode='POS', peak.mztheo=112.07569, peak.comp='P9Z6W410 O',
        peak.attr='[(M+H)-(H2O)-(NH3)]+', formula="J114L6M62O2",
        molecular.mass=146.10553, name='Blablaine'),
        stringsAsFactors=FALSE)
    conn <- biodb$getFactory()$createConn('mass.csv.file')
    conn$setDb(db.df)

    # Get entry
    entry <- conn$getEntry(id)
    testthat::expect_output(entry$print(),
        regexp='Biodb .* entry instance .*\\.')

    # Destroy connector
    biodb$getFactory()$deleteConn(conn$getId())
}

test.BiodbConn.print <- function(biodb) {

    # Get connection
    chebi.tsv <- system.file("extdata", "chebi_extract.tsv", package='biodb')
    conn <- biodb$getFactory()$createConn('comp.csv.file', url=chebi.tsv)

    # Test printing
    expect_output(conn$print(), regexp="Compound CSV File instance\\.\n  Class: comp\\.csv\\.file\\.\n  Package:.*\n  Description:.*\n  ID:.*")
}

test.BiodbDbsInfo.print <- function(biodb) {
    expect_output(biodb$getDbsInfo()$print(), regexp='^Biodb databases information instance\\.\nThe following databases are defined:.*$')
}

test.BiodbEntryFields.print <- function(biodb) {
    expect_output(biodb$getEntryFields()$print(), regexp = '^Biodb entry fields information instance\\.$')
}

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance()

# Set context
biodb::testContext("Test object printing.")

# Run tests
biodb::testThat("BiodbMain print method returns correct information.", test.BiodbMain.print, biodb = biodb)
biodb::testThat("BiodbCache print method returns correct information.", test.BiodbCache.print, biodb = biodb)
biodb::testThat("BiodbConfig print method returns correct information.", test.BiodbConfig.print, biodb = biodb)
biodb::testThat("BiodbFactory print method returns correct information.", test.BiodbFactory.print, biodb = biodb)
biodb::testThat("BiodbEntry print method returns correct information.", test.BiodbEntry.print, biodb = biodb)
biodb::testThat("BiodbConn print method returns correct information.", test.BiodbConn.print, biodb = biodb)
biodb::testThat("BiodbDbsInfo print method returns correct information.", test.BiodbDbsInfo.print, biodb = biodb)
biodb::testThat("BiodbEntryFields print method returns correct information.", test.BiodbEntryFields.print, biodb = biodb)

# Terminate Biodb
biodb$terminate()
