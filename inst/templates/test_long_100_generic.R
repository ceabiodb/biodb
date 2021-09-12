# Set test context
biodb::testContext("Generic long tests")

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance(ack=TRUE)

# Load package definitions
defFile <- system.file("definitions.yml", package='{{pkgName}}')
biodb$loadDefinitions(defFile)

# Create connector
conn <- biodb$getFactory()$createConn('{{dbName}}')

# Run generic tests
#
# TOREAD Generic tests are tests implemented inside the biodb package that are
# enough generic to run on all connectors (remote, local, compound, mass, etc).
# Among other things, they test the retrieval of entries and the parsing of the
# values of their fields. For this they need a list of reference entries with
# the expected field values. This list is stored as JSON files inside the
# "tests/testthat/res" folder. Each file must be named precisely by following
# the pattern "entry-<db_name>-<accession>.json", example:
# "entry-foo.db-0001.json". There is a file example generated for you inside
# "tests/testthat/res", edit the accession number in its name and the content
# in order to match one entry of your choice from your database. You can create
# as much entry JSON files as you want inside "tests/testthat/res", all will be
# tested by the generic tests.
# When running, generic tests create a folder "tests/testthat/output" where
# they put the downloaded entries as JSON files, in the same format as the
# files you wrote in "tests/testthat/res". This means that if you did mistake
# inside your JSON files, you can copy correct values from files in
# "tests/testthat/output" or even copy whole files to "tests/testthat/res".
#
# IMPORTANT Once you are done with the JSON files, uncomment the following line
# in order to enable generic tests to run:
#biodb::runGenericTests(conn, pkgName="{{pkgName}}", short=FALSE, long=TRUE,
#    opt=list(max.results=1))

# Terminate Biodb
biodb$terminate()
