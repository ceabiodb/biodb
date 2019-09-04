# vi: fdm=marker

source('common.R', local=TRUE)

# MAIN {{{1
################################################################

# Create biodb instance
biodb <- create.biodb.instance()
expect_is(biodb, 'Biodb')
obs <- add_msg_recorder_obs(biodb)

# Loop on test databases
for (db.name in TEST.DATABASES) {

	set.test.context(biodb, paste0("Test ", db.name, "."))

	# Get instance
	conn <- create.conn.for.generic.tests(biodb = biodb, class.db = db.name)
	expect_is(conn, 'BiodbConn')

	# Delete cache entries
	biodb$getFactory()$deleteAllCacheEntries(conn$getId())

	# Generic tests
	source('db-generic-tests.R', local=TRUE)

	# Compound database testing
	source('db-compound-tests.R', local=TRUE)

	# Mass database testing
	source('db-ms-tests.R', local=TRUE)

	# Specific tests
	f <- paste('db', db.name, 'tests.R', sep='-')
	testthat::expect_true(file.exists(f))
	source(f, local=TRUE)
}

# Terminate Biodb
biodb$terminate()
