# vi: fdm=marker

source('common.R', local=TRUE)

# MAIN {{{1
################################################################

# Create biodb instance
biodb <- biodb::createBiodbTestInstance()
obs <- biodb::addMsgRecObs(biodb)

# Loop on test databases
for (db.name in TEST.DATABASES) {

    biodb::setTestContext(biodb, paste0("Test ", db.name, "."))

	# Get instance
	conn <- create.conn.for.generic.tests(biodb = biodb, class.db = db.name)

	# Generic tests
	biodb::runGenericTests(conn)

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
