# vi: fdm=marker

source('common.R')
source('db-generic-tests.R')
source('db-ms-tests.R')
source('db-hmdb-tests.R')

# MAIN {{{1
################################################################

# Create biodb instance
biodb <- create.biodb.instance()

# Initialize MassCsvFile
if (BIODB.MASS.CSV.FILE %in% TEST.DATABASES)
	init.mass.csv.file.db(biodb)

# Loop on test modes
for (mode in TEST.MODES) {

	# Configure mode
	set.mode(biodb, mode)

	# Loop on test databases
	for (db.name in TEST.DATABASES) {

		set.test.context(biodb, paste("Running tests on database", db.name, "in", mode, "mode"))
		test_that("Wrong entry gives NULL", test.wrong.entry(biodb, db.name))
		test_that("One wrong entry does not block the retrieval of good ones", test.wrong.entry.among.good.ones(biodb, db.name))
		test_that("Entry fields have a correct value", test.entry.fields(biodb, db.name))

		# Get instance
		db <- biodb$getFactory()$getConn(db.name)

		if ( ! methods::is(db, 'RemotedbConn') || mode == MODE.ONLINE || mode == MODE.QUICK.ONLINE) {

			test_that("Nb entries is positive", test.nb.entries(db))
			test_that("We can get a list of entry ids", test.entry.ids(db))

			# Test HMDB Metabolite number of entries
			if (db.name == BIODB.HMDB.METABOLITE && mode == MODE.ONLINE)
				test_that("HMDB metabolite returns enough entries ", test.hmdbmetabolite.nbentries(db))

			# Mass database testing
			if (methods::is(db, 'MassdbConn')) {
				test_that("We can retrieve a list of M/Z values", test.getMzValues(db))
				test_that("We can match M/Z peaks", test.searchMzTol(db))
				test_that("Search by precursor returns at least one match", test.searchMzTol.with.precursor(biodb, db.name))
				test_that("MSMS search returns at least one match", test.msmsSearch(biodb, db.name))

				if (db.name == BIODB.MASS.CSV.FILE) {
					test_that("MassCsvFileConn methods are correct", test.basic.mass.csv.file(biodb))
					test_that("M/Z match output contains all columns of database.", test.mass.csv.file.output.columns(biodb))
				}
			}
		}
	}
}
