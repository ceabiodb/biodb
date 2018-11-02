# vi: fdm=marker

context('Starting database tests.')

source('common.R')
source('db-generic-tests.R')
source('db-ms-tests.R')
source('db-compound-tests.R')

source('db-chebi-tests.R')
source('db-chemspider-tests.R')
source('db-expasy.enzyme-tests.R')
source('db-hmdb-tests.R')
source('db-kegg.compound-tests.R')
source('db-lipidmaps-tests.R')
source('db-mass.csv.file-tests.R')
source('db-massbank-tests.R')
source('db-mirbase-tests.R')
source('db-peakforest-tests.R')
source('db-uniprot-tests.R')

# MAIN {{{1
################################################################

# Create biodb instance
biodb <- create.biodb.instance()
expect_is(biodb, 'Biodb')

# Initialize database connectors
connectors <- list()
for (db.name in TEST.DATABASES) {
	if (db.name == BIODB.MASS.CSV.FILE)
		conn <- init.mass.csv.file.db(biodb)
	else
		conn <- biodb$getFactory()$createConn(db.name)
	expect_is(conn, 'BiodbConn')
	connectors[[db.name]] <- conn
}

# Loop on test modes
for (mode in TEST.MODES) {

	# Configure mode
	set.mode(biodb, mode)

	# Loop on test databases
	for (db.name in TEST.DATABASES) {

		# Get instance
		conn <- connectors[[db.name]]

		# Generic tests
		run.db.generic.tests(conn, mode)

		# Compound database testing
		run.compound.db.tests(conn, mode)

		# Mass database testing
		run.mass.db.tests(conn, mode)

		# Specific tests
		fct <- paste('run', db.name, 'tests', sep = '.')
		if (exists(fct)) {
			set.test.context(biodb, paste("Running specific tests on database", conn$getName(), "in", mode, "mode"))
			do.call(fct, list(conn, mode))
		}
	}
}

# Terminate Biodb
biodb$terminate()
