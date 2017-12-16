# vi: fdm=marker

source('common.R')
source('db-generic-tests.R')
source('db-ms-tests.R')
source('db-compound-tests.R')

source('db-chebi-tests.R')
source('db-chemspider-tests.R')
source('db-expasy.enzyme-tests.R')
source('db-hmdb-tests.R')
source('db-kegg.compound-tests.R')
source('db-mass.csv.file-tests.R')
source('db-massbank-tests.R')
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

			# Generic tests
			run.db.generic.tests(db)

			# Compound database testing
			if (methods::is(db, 'CompounddbConn'))
				run.compound.db.tests(db)

			# Mass database testing
			if (methods::is(db, 'MassdbConn'))
				run.mass.db.tests(db)

			# Specific tests
			fct <- paste('run', db.name, 'tests', sep = '.')
			if (exists(fct))
				do.call(fct, list(db, mode))
		}
	}
}
