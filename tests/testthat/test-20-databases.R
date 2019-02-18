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

# Initialize all database connectors
# We need to create all connectors now, because they must be set with the proper cache ID. Connectors like ChEBI, even if not tested directly, may be used through computed fields from other connectors. Proper cache ID is required for offline tests.
connectors <- list()
for (db.name in biodb$getDbsInfo()$getIds()) {
	if (db.name == 'mass.csv.file')
		conn <- init.mass.csv.file.db(biodb)
	else if (db.name == 'mass.sqlite')
		conn <- init.mass.sqlite.db(biodb)
	else
		conn <- get.default.db(biodb, db.name)
	expect_is(conn, 'BiodbConn')
	connectors[[db.name]] <- conn
}

# Loop on test databases
for (db.name in TEST.DATABASES) {

	set.test.context(biodb, paste0("Test ", db.name, "."))

	# Loop on test modes
	for (mode in TEST.MODES) {

# TODO call get.default.db() HERE:
		# 1. erase all connectors
		# 2. call get.default.db() and pass it `mode`
		# 3. get.default.db() will set cache.id = class.db only if mode is offline
		# 4. get.default.db() will also instantiate databases that are inside getComputableFrom() if mode is offline.
		# 5. Rename get.default.db() in create.conn.for.generic.tests(). For that remove use of get.default.db() in other tests, and replace the connector used (ChEBI or Massbank) with a MassCsvFile instance.

		# Configure mode
		set.mode(biodb, mode)

		# Get instance
		conn <- connectors[[db.name]]

		# Delete cache entries
		biodb$getFactory()$deleteAllCacheEntries(conn$getId())

		# Generic tests
		run.db.generic.tests(conn, mode)

		# Compound database testing
		run.compound.db.tests(conn, mode)

		# Mass database testing
		run.mass.db.tests(conn, mode)

		# Specific tests
		fct <- paste('run', db.name, 'tests', sep = '.')
		if (exists(fct))
			do.call(fct, list(conn, mode))
	}
}

# Terminate Biodb
biodb$terminate()
