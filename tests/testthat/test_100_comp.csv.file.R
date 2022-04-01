CHEBI_FILE <- system.file("extdata", "chebi_extract.tsv", package="biodb")
CHEBI_FILE_UNKNOWN_COL <- system.file("extdata",
                                      "chebi_extract_with_unknown_column.tsv",
                                      package="biodb")

test.comp.csv.file.dynamic.field.set <- function(biodb) {

    # Create connector
    conn <- biodb$getFactory()$createConn('comp.csv.file',
                                          url=CHEBI_FILE_UNKNOWN_COL)
    
    msg <- "^.*Column \"elecCharge\" does not match any biodb field\\.$"
    testthat::expect_warning(x <- conn$getEntry('1932')$getFieldsAsDataframe(),
                             msg, perl=TRUE)
    testthat::expect_false('charge' %in% colnames(x))
    conn$setField('charge', 'elecCharge')
    
    # We must first remove the entry from all caches
    conn$deleteAllEntriesFromPersistentCache()

    x <- conn$getEntry('1932')$getFieldsAsDataframe()
    testthat::expect_true('charge' %in% colnames(x))
    
    # Delete connector
    biodb$getFactory()$deleteConn(conn)
}

test_unmapped_col <- function(biodb) {

    # Create connector
    conn <- biodb$getFactory()$createConn('comp.csv.file',
                                          url=CHEBI_FILE_UNKNOWN_COL)
    
    msg <- "^.*Column \"elecCharge\" does not match any biodb field\\.$"
    testthat::expect_warning(conn$getEntryIds(), msg, perl=TRUE)
    testthat::expect_length(conn$getUnassociatedColumns(), 1)
    testthat::expect_true(length(conn$getFieldsAndColumnsAssociation()) > 0)
    msg <- paste0("^.*The following fields have been defined:.*",
                  "Unassociated columns: elecCharge\\..*$")
    testthat::expect_output(conn$print(), msg)

    # Re-create connector
    biodb$getFactory()$deleteConn(conn)
    conn <- biodb$getFactory()$createConn('comp.csv.file',
                                          url=CHEBI_FILE_UNKNOWN_COL)
    
    conn$setIgnoreUnassignedColumns(TRUE)
    conn$getEntryIds()

    # Re-create connector
    biodb$getFactory()$deleteConn(conn)
    conn <- biodb$getFactory()$createConn('comp.csv.file',
                                          url=CHEBI_FILE_UNKNOWN_COL)

    # Define missing column
    conn$setField('charge', 'elecCharge')
    testthat::expect_length(conn$getUnassociatedColumns(), 0)

    # No warning should be issued
    conn$getEntryIds()
    
    # Delete connector
    biodb$getFactory()$deleteConn(conn)
}

test_comp.csv.file_wrong_field <- function(biodb) {
    compUrl <- system.file("extdata", "chebi_extract_custom.csv",
        package='biodb')
    compdb <- biodb$getFactory()$createConn('comp.csv.file', url=compUrl)
    testthat::expect_is(compdb, 'CompCsvFileConn')
    compdb$setCsvSep(';')
    testthat::expect_warning(cols <- compdb$getUnassociatedColumns())
        # Load CSV file inside data frame
    testthat::expect_is(cols, 'character')
    testthat::expect_length(cols, 3) # ID, molmass, kegg
    compdb$setField('accession', 'ID')
    compdb$setField('kegg.compound.id', 'kegg')
    testthat::expect_error(compdb$setField('molecular.mass', 'molmass'))
    # molecular.mass has already been mapped with 'mass', which is in fact
    # the monoisotopic mass.
    
    biodb$getFactory()$deleteConn(compdb)
    compdb <- biodb$getFactory()$createConn('comp.csv.file', url=compUrl)
    compdb$setCsvSep(';')
    compdb$setField('accession', 'ID')
    compdb$setField('kegg.compound.id', 'kegg')
    compdb$setField('monoisotopic.mass', 'mass')
    compdb$setField('molecular.mass', 'molmass')
    cols <- compdb$getUnassociatedColumns() # Load CSV file inside data frame
    testthat::expect_is(cols, 'character')
    testthat::expect_length(cols, 0)
    ids <- compdb$getEntryIds()
    testthat::expect_is(ids, 'character')
    testthat::expect_true(length(ids) > 5)
}

# Set context
biodb::testContext("CompCsvFile generic tests")

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance()

# TODO How to test this connector with both chebi and uniprot extracts?
# All entry-*.json are named after the connector name.
# Make runGenericTests() a class in which we can set the files pattern to match.
# Then listTestRefEntries() uses this pattern.

# Create connector
conn <- biodb$getFactory()$createConn('comp.csv.file', url=CHEBI_FILE)

# Run generic tests
biodb::runGenericTests(conn, pkgName='biodb')

# Run specific tests
biodb::testContext("CompCsvFile specific tests")
biodb::testThat('We receive a warning for unmapped columns.',
                test_unmapped_col, biodb=biodb)
biodb::testThat('We can define a new field even after loading an entry.',
                test.comp.csv.file.dynamic.field.set, biodb=biodb)
biodb::testThat('We can redefine wrongly mapped field.',
                test_comp.csv.file_wrong_field, biodb=biodb)

# Terminate Biodb
biodb$terminate()
