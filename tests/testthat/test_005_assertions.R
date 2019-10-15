# vi: fdm=marker

# Test assert.positive {{{1
################################################################

test.assertPositive <- function(biodb, obs) {

	for (mz in c(10, 0))
		biodb$.assertPositive(mz)
	mz <- -1
	expect_error(biodb$.assertPositive(mz))
	expect_equal(obs$getLastMsg(), "mz (-1) cannot be negative.")
}

# Test assert.in {{{1
################################################################

test.assertIn <- function(biodb, obs) {

	biodb$.assertIn('aaa', c('aaa', 'bbb'))
	str <- 'ccc'
	expect_error(biodb$.assertIn(str, c('aaa', 'bbb')))
	expect_equal(obs$getLastMsg(), "str cannot be set to ccc. Allowed values are: aaa, bbb.")
}

# Test assert.not.na {{{1
################################################################

test.assertNotNa <- function(biodb, obs) {

	biodb$.assertNotNa(10)
	biodb$.assertNotNa(c(1, 3, 10))
	for (myvar in list(NA_real_, c(1.0, NA_real_))) {
		expect_error(biodb$.assertNotNa(myvar))
		expect_equal(obs$getLastMsg(), "myvar cannot be set to NA.")
	}
}

# Test assert.not.null {{{1
################################################################

test.assertNotNull <- function(biodb, obs) {

	biodb$.assertNotNull(10)
	myvar <- NULL
	expect_error(biodb$.assertNotNull(myvar))
	expect_equal(obs$getLastMsg(), "myvar cannot be NULL.")
}

# Test assert.inferior {{{1
################################################################

test.assertInferior <- function(biodb, obs) {

	small <- 10
	big <- 20
	biodb$.assertInferior(small, big)
	myvar <- NULL
	expect_error(biodb$.assertInferior(big, small))
	expect_equal(obs$getLastMsg(), "big (20) cannot be greater than small (10).")
}

# Test assert.length.one {{{1
################################################################

test.assertLengthOne <- function(biodb, obs) {

	biodb$.assertLengthOne(10)
	myvar <- c(10, 20)
	expect_error(biodb$.assertLengthOne(myvar))
	expect_equal(obs$getLastMsg(), "Length of myvar (2) must be one.")
}

# Test searchMsEntries assert {{{1
################################################################

test.searchMsEntries.assert <- function(biodb, obs) {

	# Create database and connector
	db.df <- rbind(data.frame(), list(accession = 'C1', ms.mode = 'POS', peak.mztheo = 112.07569, peak.comp = 'P9Z6W410 O', peak.attr = '[(M+H)-(H2O)-(NH3)]+', formula = "J114L6M62O2", molecular.mass = 146.10553, name = 'Blablaine'), stringsAsFactors = FALSE)
	conn <- biodb$getFactory()$createConn('mass.csv.file')
	conn$setDb(db.df)

	# Call searchMsEntries() with wrong ms.level
	expect_error(ids <- conn$searchMsEntries(mz = 10, mz.tol = 0.01, mz.tol.unit = 'plain', max.results = 1, ms.level = -1))
	expect_equal(obs$getLastMsg(), "ms.level (-1) cannot be negative.")

	# Destroy connector
	biodb$getFactory()$deleteConn(conn$getId())
}

# Main {{{1
################################################################

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance()
obs <- biodb::addMsgRecObs(biodb)

# Set context
biodb::setTestContext(biodb, "Test assertions.")

# Run tests
biodb::testThat("Assertion of positive number works correctly", test.assertPositive, biodb = biodb, obs = obs)
biodb::testThat("Assertion of enumerate works correctly", test.assertIn, biodb = biodb, obs = obs)
biodb::testThat("Assertion of non NA value works correctly", test.assertNotNa, biodb = biodb, obs = obs)
biodb::testThat("Assertion of non NULL value works correctly", test.assertNotNull, biodb = biodb, obs = obs)
biodb::testThat("Assertion of inferior relationship works correctly", test.assertInferior, biodb = biodb, obs = obs)
biodb::testThat("Assertion of a single element works correctly", test.assertLengthOne, biodb = biodb, obs = obs)
biodb::testThat('Assertion called from searchMsEntries display the right variable name', test.searchMsEntries.assert, biodb = biodb, obs = obs)

# Terminate Biodb
biodb$terminate()
