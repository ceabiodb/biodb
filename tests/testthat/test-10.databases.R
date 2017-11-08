# vi: fdm=marker

source('common.R')
source('db-generic-tests.R')
source('db-ms-tests.R')
source('db-compound-tests.R')
source('db-hmdb-tests.R')
source('db-chebi-tests.R')
source('db-uniprot-tests.R')

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

		# Get instance
		db <- biodb$getFactory()$getConn(db.name)

		set.test.context(biodb, paste("Running tests on database", db.name, "in", mode, "mode"))
		run.db.test("Wrong entry gives NULL", 'test.wrong.entry', db)
		run.db.test("One wrong entry does not block the retrieval of good ones", 'test.wrong.entry.among.good.ones', db)
		run.db.test("Entry fields have a correct value", 'test.entry.fields', db)

		if ( ! methods::is(db, 'RemotedbConn') || mode %in% c(MODE.ONLINE, MODE.QUICK.ONLINE)) {

			run.db.test("Nb entries is positive", 'test.nb.entries', db)
			run.db.test("We can get a list of entry ids", 'test.entry.ids', db)

			# Test HMDB Metabolite number of entries
			if (db.name == BIODB.HMDB.METABOLITE && mode %in% c(MODE.ONLINE, MODE.QUICK.ONLINE))
				run.db.test("HMDB metabolite returns enough entries ", 'test.hmdbmetabolite.nbentries', db)

			# Test ChEBI
			if (db.name == 'chebi' && mode %in% c(MODE.ONLINE, MODE.QUICK.ONLINE))
				run.db.test('ChEBI encoding issue in XML is handled.', 'test.chebi.encoding.issue.in.xml', db)

			# Test Uniprot
			if (db.name == 'uniprot' && mode %in% c(MODE.ONLINE, MODE.QUICK.ONLINE))
				run.db.test('Uniprot entries query works fine.', 'test.uniprot.ws.query', db)

			# Compound database testing
			if (methods::is(db, 'CompounddbConn')) {
				run.db.test('We can search for a compound', 'test.searchCompound', db)
				if (db.name == 'chebi') {
					run.db.test("Test bug 20170926.01 for ChEBI.", 'test.chebi.searchCompound.bug.20170926.01', db)
				}
			}

			# Mass database testing
			if (methods::is(db, 'MassdbConn')) {
				run.db.test("We can retrieve a list of M/Z values", 'test.getMzValues', db)
				run.db.test("We can match M/Z peaks", 'test.searchMzTol',db)
				run.db.test("We can search for spectra containing several M/Z values", 'test.searchMzTol.multiple.mz',db)
				run.db.test("Search by precursor returns at least one match", 'test.searchMzTol.with.precursor', db)
				run.db.test("MSMS search can find a match for a spectrum from the database itself.", 'test.msmsSearch.self.match', db)
				if (db.name == 'massbank.jp' && mode %in% c(MODE.ONLINE, MODE.QUICK.ONLINE))
					run.db.test('MSMS search works for massbank.', 'test.msmsSearch.massbank', db)
				run.db.test('MSMS search works for an empty spectrum.', 'test.msmsSearch.empty.spectrum', db)
				run.db.test('MSMS search works for a null spectrum.', 'test.msmsSearch.null.spectrum', db)
				run.db.test("The peak table is correct.", 'test.peak.table', db)

				if (db.name == BIODB.MASS.CSV.FILE) {
					run.db.test("MassCsvFileConn methods are correct", 'test.basic.mass.csv.file', db)
					run.db.test("M/Z match output contains all columns of database.", 'test.mass.csv.file.output.columns', db)
				}
			}
		}
	}
}
