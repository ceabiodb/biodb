test_annotateWithMassCsvFileDb <- function(biodb) {
    mzdf <- data.frame(mz=c(282.083871, 283.0623, 346.0546, 821.3964),
                       rt=c(334, 872, 536, 740))
    lcmsdb <- system.file("extdata", "massbank_extract_lcms_1.tsv",
                          package="biodb")
    massbank <- biodb$getFactory()$createConn('mass.csv.file', url=lcmsdb)
    massbank$addField('ms.level', 1)
    massbank$addField('chrom.rt.unit', 's')
    massbank$searchMsPeaks(mzdf, mz.tol=1e-3, fields=c('accession', 'name',
                                                       'formula', 'chebi.id'),
                           prefix='mydb.')
    chromColIds <- c('TOSOH TSKgel ODS-100V  5um Part no. 21456')
    fields <- c('accession', 'name', 'formula', 'chebi.id', 'chrom.rt',
                'chrom.col.id')
    massbank$searchMsPeaks(mzdf, mz.tol=1e-3, fields=fields, prefix='mydb.',
                           chrom.col.ids=chromColIds, rt.unit='s', rt.tol=10,
                           match.rt=TRUE)
}

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance(log='lcms_annotation_test.log')

# Set context
biodb::setTestContext(biodb, "Test LCMS annotation.")

# Run tests
biodb::testThat("We can annotate using mass CSV file db.",
                test_annotateWithMassCsvFileDb, biodb=biodb)

# Terminate Biodb
biodb$terminate()
