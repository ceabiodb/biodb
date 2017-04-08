# vi: fdm=marker

source('common.R')

# Test msmsSearch {{{1
################################################################

test.msmsSearch <- function(biodb, db.name) {

	db <- biodb$getFactory()$getConn(db.name)

	# Loop on modes
	for (mode in BIODB.MSMODE.VALS) {

		# Get one mz value
		mz <- db$getMzValues(ms.mode = mode, ms.level = 2, max.results = 1, precursor = TRUE)

		# Found corresponding spectrum
		print(mz)
		spectrum.id <- db$searchMzTol(mz, tol = 5, tol.unit = BIODB.MZTOLUNIT.PPM, ms.mode = mode, max.results = 1, ms.level = 2)

		# Get entry
		spectrum.entry <- biodb$getFactory()$getEntry(db.name, spectrum.id)

		# Get peaks
		peaks <- spectrum.entry$getFieldValue(BIODB.PEAKS)

		# Run MSMS search
		result <- db$msmsSearch(peaks, precursor = mz, mztol = 0.1, tolunit = BIODB.MZTOLUNIT.PLAIN, mode = mode, npmin = 2, fun = 'pbachtttarya', params = list(ppm = 3, dmz = 0.005, mzexp = 2, intexp = 0.5))

		# Check results
		expect_true( ! is.null(result))
		expect_true(length(result$matchedpeaks) > 0)
	}
}

# Test getMzValues() {{{1
################################################################

test.getMzValues <- function(db) {
	max <- 10
	for (mode in c(BIODB.MSMODE.NEG, BIODB.MSMODE.POS)) {
		mz <- db$getMzValues(ms.mode = mode, max.results = max)
		expect_true(is.double(mz))
		n <- length(mz)
		expect_true(n >= 1 && n <= max)
	}
}

# Test searchMzTol() {{{1
################################################################

test.searchMzTol <- function(db) {

	# Get M/Z values from database
	mode <- BIODB.MSMODE.POS
	mzs <- db$getMzValues(ms.mode = mode, max.results = 10)
	expect_true(is.double(mzs))
	expect_true(length(mzs) >= 1)

	# Search
	for (mz in mzs) {
		ids <- db$searchMzTol(mz = mz, tol = 5, tol.unit = BIODB.MZTOLUNIT.PLAIN, min.rel.int = 0, ms.mode = mode)
		expect_true(is.character(ids))
		expect_true(length(ids) > 0)
	}
}

# Test searchMzTol() with precursor {{{1
################################################################

test.searchMzTol.with.precursor <- function(biodb, db.name) {

	db <- biodb$getFactory()$getConn(db.name)

	# Loop on levels
	for (ms.level in c(1, 2)) {

		# Get an M/Z value of a precursor
		mz <- db$getMzValues(precursor = TRUE, max.results = 1, ms.level = ms.level)
		expect_equal(length(mz), 1)

		# Search for it
		spectra.ids <- db$searchMzTol(mz = mz, tol = 5, tol.unit = BIODB.MZTOLUNIT.PPM, precursor = TRUE, ms.level = ms.level)
		expect_gte(length(spectra.ids), 1)
	}
}

# Main {{{1
################################################################

# Create biodb instance
biodb <- create.biodb.instance()

set.test.context(biodb, "Testing MS features")

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

		if ( ! methods::is(db, 'RemotedbConn') || mode == MODE.ONLINE || mode == MODE.QUICK.ONLINE) {

			# Mass database testing
			if (methods::is(db, 'MassdbConn')) {
				test_that("We can retrieve a list of M/Z values", test.getMzValues(db))
				test_that("We can match M/Z peaks", test.searchMzTol(db))
				test_that("Search by precursor returns at least one match", test.searchMzTol.with.precursor(biodb, db.name))
				test_that("MSMS search returns at least one match", test.msmsSearch(biodb, db.name))
			}
		}
	}
}
