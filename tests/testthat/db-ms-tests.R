# vi: fdm=marker

# Test msmsSearch self match {{{1
################################################################

test.msmsSearch.self.match <- function(db) {

	biodb <- db$getBiodb()
	db.name <- db$getId()

	# Set some initial values to speed up test
	db.values <- list(peakforest.mass = list(neg = NULL, pos = list(spectrum.id = '3828', mz = 117.1)),
	                  massbank.jp = list(neg = list(spectrum.id = 'PR100504', mz = 193.0354), pos = list(spectrum.id = 'AU106501', mz = 316.075)))

	# Loop on modes
	for (mode in c('neg', 'pos')) {

		# Get M/Z value and spectrum ID to be tested
		if (db.name %in% names(db.values) && mode %in% names(db.values[[db.name]])) {
			if (is.null(db.values[[db.name]][[mode]]))
				next
			mz <- db.values[[db.name]][[mode]]$mz
			spectrum.id <- db.values[[db.name]][[mode]]$spectrum.id
		}
		else {
			# Search for one M/Z value
			mz <- db$getMzValues(ms.mode = mode, ms.level = 2, max.results = 1, precursor = TRUE)

			# Find corresponding spectrum
			spectrum.id <- db$searchMzTol(mz, mz.tol = 5, mz.tol.unit = 'ppm', ms.mode = mode, max.results = 1, ms.level = 2, precursor = TRUE)
		}

		# Get entry
		spectrum.entry <- biodb$getFactory()$getEntry(db.name, spectrum.id)

		# Get peaks
		peaks <- spectrum.entry$getFieldValue('peaks')
		int.col <- if ('peak.intensity' %in% names(peaks)) 'peak.intensity' else 'peak.relative.intensity'
		peaks <- peaks[order(peaks[[int.col]], decreasing = TRUE), ]
		peaks <- peaks[1:2, ]

		# Run MSMS search
		results <- db$msmsSearch(peaks, precursor = mz, mz.tol = 0.1, mz.tol.unit = BIODB.MZTOLUNIT.PLAIN, ms.mode = mode, npmin = 2, dist.fun = 'pbachtttarya', msms.mz.tol = 3, msms.mz.tol.min = 0.005)

		# Check results
		expect_true( ! is.null(results))
		expect_true(is.data.frame(results))
		expect_true(nrow(results) > 0)
		cols <- c('id', 'score', paste('peak', seq(nrow(peaks)), sep = '.'))
		expect_true(all(cols %in% colnames(results)))
		expect_true(spectrum.id %in% results[['id']])
	}
}

# Test msmsSearch empty spectrum {{{1
################################################################

test.msmsSearch.empty.spectrum <- function(db) {

	# Define spectrum to match:
	spectrum <- data.frame(mz = numeric(0), rel.int = numeric(0))

	# Search for match:
	result <- db$msmsSearch(spectrum, precursor.mz = 100, mz.tol = 0.3)

	expect_true( ! is.null(result))
	expect_true(is.data.frame(result))
	expect_true(nrow(result) == 0)
	cols <- c('id', 'score')
	expect_true(all(cols %in% colnames(result)))
}

# Test msmsSearch null spectrum {{{1
################################################################

test.msmsSearch.null.spectrum <- function(db) {

	# Search for match:
	result <- db$msmsSearch(NULL, precursor.mz = 100, mz.tol = 0.3)

	expect_true( ! is.null(result))
	expect_true(is.data.frame(result))
	expect_true(nrow(result) == 0)
	cols <- c('id', 'score')
	expect_true(all(cols %in% colnames(result)))
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
	mzs <- db$getMzValues(ms.mode = mode, max.results = 2)
	expect_true(is.double(mzs))
	expect_true(length(mzs) >= 1)

	# Search
	for (mz in mzs) {
		ids <- db$searchMzTol(mz = mz, mz.tol = 5, mz.tol.unit = BIODB.MZTOLUNIT.PLAIN, min.rel.int = 0, ms.mode = mode)
		expect_true(is.character(ids))
		expect_true(length(ids) > 0)
	}
}

# Test searchMzTol() with multiple M/Z values {{{1
################################################################

test.searchMzTol.multiple.mz <- function(db) {

	mz.tol <- 0.0001

	# Get M/Z values from database
	mode <- BIODB.MSMODE.POS
	mzs <- db$getMzValues(ms.mode = mode, max.results = 2)
	expect_true(is.double(mzs))
	expect_true(length(mzs) >= 1)

	# Search one M/Z at a time
	all.ids <- character(0)
	for (mz in mzs) {
		ids <- db$searchMzTol(mz = mz, mz.tol = mz.tol, mz.tol.unit = BIODB.MZTOLUNIT.PLAIN, min.rel.int = 0, ms.mode = mode)
		expect_true(is.character(ids))
		expect_true(length(ids) > 0)
		all.ids <- c(all.ids, ids)
	}
	all.ids <- all.ids[ ! duplicated(all.ids)]

	# Search all M/Z values at once
	all.ids.2 <- db$searchMzTol(mz = mzs, mz.tol = mz.tol, mz.tol.unit = BIODB.MZTOLUNIT.PLAIN, min.rel.int = 0, ms.mode = mode)

	# List of IDs must be the same
	expect_true(all(all.ids.2 %in% all.ids))
}

# Test searchMzTol() with precursor {{{1
################################################################

test.searchMzTol.with.precursor <- function(db) {

	biodb <- db$getBiodb()
	db.name <- db$getId()

	# Set some initial values to speed up test
	db.values <- list(massbank.jp = list('1' = list(mz = 313.3), '2' = list(mz = 285.0208)),
	                  peakforest.mass = list('2' = list(mz = 117.1)))

	db <- biodb$getFactory()$getConn(db.name)
	tol.ppm <- 5

	# Loop on levels
	for (ms.level in c(1, 2)) {

		# Get an M/Z value of a precursor
		if (db.name %in% names(db.values) && as.character(ms.level) %in% names(db.values[[db.name]]))
			mz <- db.values[[db.name]][[as.character(ms.level)]]$mz
		else
			mz <- db$getMzValues(precursor = TRUE, max.results = 1, ms.level = ms.level)
		expect_false(is.null(mz))
		expect_length(mz, 1)
		expect_false(is.na(mz))

		# Search for it
		spectra.ids <- db$searchMzTol(mz = mz, mz.tol = tol.ppm, mz.tol.unit = BIODB.MZTOLUNIT.PPM, precursor = TRUE, ms.level = ms.level)
		expect_gte(length(spectra.ids), 1)
		expect_false(any(is.na(spectra.ids)))

		# Get first entry
		for (spectra.id in spectra.ids) {
			entry <- biodb$getFactory()$getEntry(db.name, spectra.id)
			expect_false(is.null(entry))
			expect_false(is.na(entry$getFieldValue('ms.level')))
			expect_equal(entry$getFieldValue('ms.level'), ms.level)
			peaks <- entry$getFieldValue('peaks')
			expect_false(is.null(peaks))
			expect_true(is.data.frame(peaks))
			expect_gt(nrow(peaks), 0)
			expect_true('peak.mz' %in% colnames(peaks))

			# Check that precursor peak was matched
			expect_true(entry$hasField('msprecmz'))
			expect_true(abs(entry$getFieldValue('msprecmz') - mz) < mz * tol.ppm * 1e-6)
		}
	}
}


# Test getChromCol {{{1
################################################################

test.getChromCol <- function(db) {
	chrom.col <- db$getChromCol(ids = list.ref.entries(db$getId()))
	expect_is(chrom.col, 'data.frame')
	expect_identical(names(chrom.col), c('id', 'title'))
}

# Test searchMsPeaks {{{1
################################################################

test.searchMsPeaks <- function(db) {

	mode <- 'neg'
	tol <- 0.1

	mzs <- db$getMzValues(ms.mode = mode, max.results = 3)

	# Get only one result per M/Z value
	results <- db$searchMsPeaks(mzs, mz.tol = tol, max.results = 1, ms.mode = mode)
	expect_is(results, 'data.frame')
	expect_true(nrow(results) >= 1)
	expect_true('accession' %in% names(results))
	expect_true('peak.mz' %in% names(results))
 	expect_true(all(vapply(mzs, function(mz) any((results$peak.mz >= mz - tol) & (results$peak.mz <= mz + tol)), FUN.VALUE = TRUE)))

	# Get 2 results per M/Z value
	results <- db$searchMsPeaks(mzs, mz.tol = tol, max.results = 2, ms.mode = mode)
	expect_is(results, 'data.frame')
	expect_true(nrow(results) > 1)
	expect_true('accession' %in% names(results))
	expect_true('peak.mz' %in% names(results))
 	expect_true(all(vapply(mzs, function(mz) any((results$peak.mz >= mz - tol) & (results$peak.mz <= mz + tol)), FUN.VALUE = TRUE)))
}

# Test msmsSearch whe no IDs are found {{{1
################################################################

test.msmsSearch.no.ids <- function(db) {
	tspec <- data.frame(mz = 1, int = 10000)
	results <- db$msmsSearch(tspec, precursor.mz = 2, mz.tol = 0.5, mz.tol.unit = 'plain', ms.mode = "pos",msms.mz.tol = 10,msms.mz.tol.min = 0.01,npmin = 2)
	expect_is(results, 'data.frame')
	expect_equal(nrow(results), 0)
	expect_true(all(c('id', 'score') %in% names(results)))
}

# Run Mass DB tests {{{1
################################################################

run.mass.db.tests <- function(db, mode) {
	if ( ! methods::is(db, 'RemotedbConn') || mode %in% c(MODE.ONLINE, MODE.QUICK.ONLINE))
		if (methods::is(db, 'MassdbConn')) {

			set.test.context(db$getBiodb(), paste("Running M/Z search generic tests on database", db$getId(), "in", mode, "mode"))
			run.db.test.that("We can retrieve a list of M/Z values.", 'test.getMzValues', db)
			run.db.test.that("We can match M/Z peaks.", 'test.searchMzTol',db)
			run.db.test.that("We can search for spectra containing several M/Z values.", 'test.searchMzTol.multiple.mz',db)
			run.db.test.that("Search by precursor returns at least one match.", 'test.searchMzTol.with.precursor', db)

			set.test.context(db$getBiodb(), paste("Running LCMS generic tests on database", db$getId(), "in", mode, "mode"))
			run.db.test.that("We can retrieve a list of chromatographic columns.", 'test.getChromCol', db)
			run.db.test.that("We can search for several M/Z values, separately.", 'test.searchMsPeaks', db)

			set.test.context(db$getBiodb(), paste("Running MSMS generic tests on database", db$getId(), "in", mode, "mode"))
			run.db.test.that("MSMS search can find a match for a spectrum from the database itself.", 'test.msmsSearch.self.match', db)
			run.db.test.that('MSMS search works for an empty spectrum.', 'test.msmsSearch.empty.spectrum', db)
			run.db.test.that('MSMS search works for a null spectrum.', 'test.msmsSearch.null.spectrum', db)
			run.db.test.that('No failure occurs when msmsSearch found no IDs.', 'test.msmsSearch.no.ids', db)
		}
}
