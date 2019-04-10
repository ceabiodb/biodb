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
source('db-kegg.pathway-tests.R')
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
obs <- create.test.observer(biodb)

# Loop on test databases
for (db.name in TEST.DATABASES) {

	set.test.context(biodb, paste0("Test ", db.name, "."))

	# Get instance
	conn <- create.conn.for.generic.tests(biodb = biodb, class.db = db.name)
	expect_is(conn, 'BiodbConn')

	# Delete cache entries
	biodb$getFactory()$deleteAllCacheEntries(conn$getId())

	# Generic tests
	run.db.generic.tests(conn)

	# Compound database testing
	run.compound.db.tests(conn)

	# Mass database testing
	run.mass.db.tests(conn)

	# Specific tests
	fct <- paste('run', db.name, 'tests', sep = '.')
	if (exists(fct))
		do.call(fct, list(conn = conn, obs = obs))
}

# Terminate Biodb
biodb$terminate()
