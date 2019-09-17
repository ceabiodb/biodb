# vi: fdm=marker

# Test msmsSearch self match {{{1
################################################################

test.msmsSearch.self.match <- function(db) {

	biodb <- db$getBiodb()
	db.name <- db$getId()

	# Set some initial values to speed up test
	db.values <- list(peakforest.mass = list(neg = NULL, pos = list(spectrum.id = '3828', mz = 117.1)),
	                  massbank = list(neg = list(spectrum.id = 'PR100504', mz = 193.0354), pos = list(spectrum.id = 'AU106501', mz = 316.075)))

	# Loop on distance functions
	for (dist.fct in c('wcosine', 'cosine', 'pkernel', 'pbachtttarya'))

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
			results <- db$msmsSearch(peaks, precursor = mz, mz.tol = 0.1, mz.tol.unit = 'plain', ms.mode = mode, npmin = 2, dist.fun = dist.fct, msms.mz.tol = 3, msms.mz.tol.min = 0.005)

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
	for (mode in c('neg', 'pos')) {
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
	mode <-'pos' 
	mzs <- db$getMzValues(ms.mode = mode, max.results = 2)
	expect_true(is.double(mzs))
	expect_true(length(mzs) >= 1)

	# Search
	for (mz in mzs) {
		ids <- db$searchMzTol(mz = mz, mz.tol = 5, mz.tol.unit = 'plain', min.rel.int = 0, ms.mode = mode)
		expect_true(is.character(ids))
		expect_true(length(ids) > 0)
	}
}

# Test searchMsEntries() with N/A value in input {{{1
################################################################

test.searchMsEntries.with.NA.value <- function(db) {

	ids <- db$searchMsEntries(mz = NA_real_, mz.tol = 5.0, mz.tol.unit = 'plain', min.rel.int = 0, ms.mode = 'pos')
	testthat::expect_is(ids, 'character')
	testthat::expect_length(ids, 0)
}

# Test searchMsPeaks() with N/A value {{{1
################################################################

test.searchMsPeaks.with.NA.value <- function(db) {

	# With only one single N/A value
	peaks <- db$searchMsPeaks(mz=NA_real_, mz.tol=5.0, mz.tol.unit='plain')
	testthat::expect_is(peaks, 'data.frame')
	testthat::expect_equal(nrow(peaks), 1)
	testthat::expect_equal(ncol(peaks), 1)
	testthat::expect_equal(colnames(peaks), 'mz')
	testthat::expect_true(is.na(peaks[['mz']]))

	# With one N/A value and one real value
	mode <- 'neg'
	tol <- 0
	mzs <- db$getMzValues(ms.mode=mode, max.results=3)
	mzs <- c(mzs, NA_real_)
	peaks <- db$searchMsPeaks(mz=mzs, mz.tol=tol, mz.tol.unit='plain',
                              ms.mode=mode, max.results=2)
	testthat::expect_is(peaks, 'data.frame')
	testthat::expect_true(nrow(peaks) >= length(mzs))
	testthat::expect_true(nrow(peaks) <= 2 * length(mzs))
	testthat::expect_true(ncol(peaks) > 1)
	testthat::expect_true(! all(is.na(peaks[1:(nrow(peaks) - 1), ])))
	testthat::expect_true(all(is.na(peaks[nrow(peaks), ])))
}

# Test searchMzTol() with multiple M/Z values {{{1
################################################################

test.searchMzTol.multiple.mz <- function(db) {

	mz.tol <- 0.0001

	# Get M/Z values from database
	mode <- 'pos'
	mzs <- db$getMzValues(ms.mode = mode, max.results = 2)
	testthat::expect_is(mzs, 'numeric')
	testthat::expect_true(length(mzs) >= 1)

	# Search one M/Z at a time
	all.ids <- character(0)
	for (mz in mzs) {
		ids <- db$searchMzTol(mz = mz, mz.tol = mz.tol, mz.tol.unit = 'plain', min.rel.int = 0, ms.mode = mode)
		testthat::expect_is(ids, 'character')
		testthat::expect_true(length(ids) > 0)
		all.ids <- c(all.ids, ids)
	}
	all.ids <- all.ids[ ! duplicated(all.ids)]

	# Search all M/Z values at once
	all.ids.2 <- db$searchMzTol(mz = mzs, mz.tol = mz.tol, mz.tol.unit = 'plain', min.rel.int = 0, ms.mode = mode)

	# List of IDs must be the same
	testthat::expect_true(all(all.ids.2 %in% all.ids))
}

# Test searchMzTol() with precursor {{{1
################################################################

test.searchMzTol.with.precursor <- function(db) {

	biodb <- db$getBiodb()
	db.name <- db$getId()

	# Set some initial values to speed up test
	db.values <- list(massbank = list('1' = list(mz = 313.3), '2' = list(mz = 285.0208)),
	                  peakforest.mass = list('2' = list(mz = 117.1)))

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
		spectra.ids <- db$searchMzTol(mz = mz, mz.tol = tol.ppm, mz.tol.unit = 'ppm', precursor = TRUE, ms.level = ms.level)
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
			expect_true(any(abs(entry$getFieldValue('msprecmz') - mz) < mz * tol.ppm * 1e-6))
		}
	}
}

#  Test searchMzTol() with precursor and multiple inputs {{{1
################################################################

test.searchMzTol.with.precursor.and.multiple.inputs <- function(db) {

	# Input values
	mz <- c(82.04819461, 83.01343941)
	mz.tol <- 5
	mz.tol.unit <- 'ppm'
	ms.level <- 0
	ms.mode <- 'pos'

	# Search
	ids <- db$searchMzTol(mz = mz, mz.tol = mz.tol, mz.tol.unit = mz.tol.unit, ms.level = ms.level, ms.mode = ms.mode, precursor = TRUE)
	testthat::expect_is(ids, 'character')
}

# Test getChromCol() {{{1
################################################################

test.getChromCol <- function(db) {
	chrom.col <- db$getChromCol(ids = list.ref.entries(db$getId()))
	expect_is(chrom.col, 'data.frame')
	expect_identical(names(chrom.col), c('id', 'title'))
	expect_gt(nrow(chrom.col), 0)
}

# Test searchMsPeaks() {{{1
################################################################

test.searchMsPeaks <- function(db) {

	mode <- 'neg'
	tol <- 0.1
	mzs <- db$getMzValues(ms.mode = mode, max.results = 3)

	# Test with empty list in input
	expect_null(db$searchMsPeaks(NULL, mz.tol = tol, max.results = 1, ms.mode = mode))
	expect_null(db$searchMsPeaks(integer(), mz.tol = tol, max.results = 1, ms.mode = mode))
	expect_null(db$searchMsPeaks(numeric(), mz.tol = tol, max.results = 1, ms.mode = mode))

	# Test with impossible M/Z value to simulate no match
	impossible.value <- 1e10
	results <- db$searchMsPeaks(impossible.value, mz.tol=tol, max.results=1,
                                ms.mode=mode,
                                prefix.on.result.cols='myprefix.')
	expect_is(results, 'data.frame')
	expect_identical(results, data.frame(mz = impossible.value))

	# Get only one result per M/Z value
	results <- db$searchMsPeaks(mzs, mz.tol = tol, max.results = 1, ms.mode = mode)
	expect_is(results, 'data.frame')
	expect_true(nrow(results) >= length(mzs))
	expect_true('accession' %in% names(results))
	expect_true('peak.mz' %in% names(results))
	expect_true(all(vapply(mzs, function(mz) any((results$peak.mz >= mz - tol) & (results$peak.mz <= mz + tol)), FUN.VALUE = TRUE)))

	# Get 2 results per M/Z value
	results <- db$searchMsPeaks(mzs, mz.tol = tol, max.results = 2, ms.mode = mode)
	expect_is(results, 'data.frame')
	expect_true(nrow(results) >  length(mzs))
	expect_true('accession' %in% names(results))
	expect_true('peak.mz' %in% names(results))
	expect_true(all(vapply(mzs, function(mz) any((results$peak.mz >= mz - tol) & (results$peak.mz <= mz + tol)), FUN.VALUE = TRUE)))

	# Test insert.input.values
	results <- db$searchMsPeaks(mzs, mz.tol = tol, max.results = 2, ms.mode = mode, insert.input.values = TRUE)
	expect_is(results, 'data.frame')
	expect_true('mz' %in% colnames(results))
	results <- db$searchMsPeaks(input.df = data.frame(mz = mzs), mz.tol = tol, max.results = 2, ms.mode = mode, insert.input.values = TRUE)
	expect_is(results, 'data.frame')
	expect_true('mz' %in% colnames(results))
	some.col = rep('xxx', length(mzs))
	results <- db$searchMsPeaks(input.df = data.frame(mz = mzs, some.col = some.col, stringsAsFactors = FALSE), mz.tol = tol, max.results = 2, ms.mode = mode, insert.input.values = TRUE)
	expect_is(results, 'data.frame')
	expect_true('mz' %in% colnames(results))
	expect_true('some.col' %in% colnames(results))
	expect_is(results[['some.col']], 'character')
	expect_true(all(results[['some.col']] == some.col[[1]]))

	# Test insert.input.values with prefix.on.result.cols
	results <- db$searchMsPeaks(input.df = data.frame(mz = mzs, some.col = some.col, stringsAsFactors = FALSE), mz.tol = tol, max.results = 2, ms.mode = mode, insert.input.values = TRUE, prefix.on.result.cols = 'myprefix.')
	expect_is(results, 'data.frame')
	expect_true('mz' %in% colnames(results))
	expect_true('some.col' %in% colnames(results))
	expect_is(results[['some.col']], 'character')
	expect_true(all(results[['some.col']] == some.col[[1]]))
}

# Test collapseResultsDataFrame() {{{1
################################################################

test.collapseResultsDataFrame <- function(db) {

	mode <- 'neg'
	tol <- 0.1
	mzs <- db$getMzValues(ms.mode = mode, max.results = 3)

	# Get 2 results per M/Z value
	results <- db$searchMsPeaks(mzs, mz.tol = tol, max.results = 2, ms.mode = mode)
	expect_is(results, 'data.frame')
	expect_true(nrow(results) >  length(mzs))

	# Get collapsed data frame
	collapsed.results <- db$collapseResultsDataFrame(results)
	expect_is(collapsed.results, 'data.frame')
	expect_equal(nrow(collapsed.results), length(mzs))
	expect_identical(collapsed.results[['mz']], mzs)
}

# Test searchMsPeaks by M/Z and RT {{{1
################################################################

test.searchMsPeaks.rt <- function(db) {

	# Get reference entries
	ids <- list.ref.entries(db$getId())
	entry <- db$getBiodb()$getFactory()$getEntry(db$getId(), ids[[1]])

	# Set retention time info
	if (entry$hasField('chrom.rt'))
		rt <- entry$getFieldValue('chrom.rt')
	else if (entry$hasField('chrom.rt.min') && entry$hasField('chrom.rt.max'))
		rt <- (entry$getFieldValue('chrom.rt.min') + entry$getFieldValue('chrom.rt.max')) / 2
	expect_is(rt, 'numeric')
	expect_false(is.na(rt))
	chrom.col.ids <- entry$getFieldValue('chrom.col.id')
	expect_is(chrom.col.ids, 'character')
	expect_false(is.na(chrom.col.ids))
	rt.unit <- entry$getFieldValue('chrom.rt.unit')
	expect_is(rt.unit, 'character')
	expect_false(is.na(rt.unit))

	# Get peak table
	peaks <- entry$getFieldValue('peaks')
	mz <- peaks[1, 'peak.mz']
	expect_is(mz, 'numeric')

	# Search for MZ/RT
	mz.tol <- 0
	rt.tol <- 0
	peaks <- db$searchMsPeaks(mz = mz, chrom.col.ids = chrom.col.ids, rt = rt, rt.tol = rt.tol, mz.tol = mz.tol, max.results = 1, ms.mode = entry$getFieldValue('ms.mode'), rt.unit = rt.unit)
	expect_is(peaks, 'data.frame')
	expect_true(nrow(peaks) > 0)
	expect_true(all((peaks$peak.mz >= mz - mz.tol) & (peaks$peak.mz <= mz + mz.tol)))
	expect_true(all((peaks$chrom.rt >= rt - rt.tol) & (peaks$chrom.rt <= rt + rt.tol)))

	# Search for MZ/RT without chrom.col.ids
	peaks <- db$searchMsPeaks(mz = mz, rt = rt, rt.tol = rt.tol, mz.tol = mz.tol, max.results = 1, ms.mode = entry$getFieldValue('ms.mode'), rt.unit = rt.unit)
	expect_is(peaks, 'data.frame')
	expect_true(nrow(peaks) > 0)
	expect_true(all((peaks$peak.mz >= mz - mz.tol) & (peaks$peak.mz <= mz + mz.tol)))
	expect_true(all((peaks$chrom.rt >= rt - rt.tol) & (peaks$chrom.rt <= rt + rt.tol)))
}

# Test msmsSearch when no IDs are found {{{1
################################################################

test.msmsSearch.no.ids <- function(db) {
	tspec <- data.frame(mz = 1, int = 10000)
	results <- db$msmsSearch(tspec, precursor.mz = 2, mz.tol = 0.5, mz.tol.unit = 'plain', ms.mode = "pos", msms.mz.tol = 10, msms.mz.tol.min = 0.01, npmin = 2)
	expect_is(results, 'data.frame')
	expect_equal(nrow(results), 0)
	expect_true(all(c('id', 'score') %in% names(results)))
}

# Test convertMzTolToRange() {{{1
################################################################

test.convertMzTolToRange <- function(db) {

	# Test plain unit
	range <- db$.convertMzTolToRange(1.0, 0.0, 0.0, 'plain')
	testthat::expect_is(range, 'list')
	testthat::expect_identical(range, list(min = 1.0, max = 1.0))

	# Test ppm unit
	range <- db$.convertMzTolToRange(1.0, 0.0, 0.0, 'ppm')
	testthat::expect_is(range, 'list')
	testthat::expect_identical(range, list(min = 1.0, max = 1.0))

	# Test NA value
	range <- db$.convertMzTolToRange(NA_real_, 0.0, 0.0, 'ppm')
	testthat::expect_is(range, 'list')
	testthat::expect_identical(range, list(min = NA_real_, max = NA_real_))
}

# Main {{{1
################################################################

if (conn$isMassdb()) {

	test.that("M/Z tolerance values are converted correctly to M/Z range.", 'test.convertMzTolToRange', conn = conn)

	test.that("We can retrieve a list of M/Z values.", 'test.getMzValues', conn = conn)
	test.that("We can match M/Z peaks.", 'test.searchMzTol',conn = conn)
	test.that("We can search for spectra containing several M/Z values.", 'test.searchMzTol.multiple.mz',conn = conn)
	test.that("Search by precursor returns at least one match.", 'test.searchMzTol.with.precursor', conn = conn)
	test.that("Search by precursor with multiple mz inputs does not fail.", 'test.searchMzTol.with.precursor.and.multiple.inputs', conn = conn)
	test.that("Search for N/A value returns an empty list.", 'test.searchMsEntries.with.NA.value', conn = conn)
	test.that("Search for peaks with N/A value returns no match.", 'test.searchMsPeaks.with.NA.value', conn = conn)

	test.that("We can retrieve a list of chromatographic columns.", 'test.getChromCol', conn = conn)
	test.that("We can search for several M/Z values, separately.", 'test.searchMsPeaks', conn = conn)
	test.that("We can collapse the results from searchMsPeaks().", 'test.collapseResultsDataFrame', conn = conn)
	test.that("We can search for several couples of (M/Z, RT) values, separately.", 'test.searchMsPeaks.rt', conn = conn)

	test.that("MSMS search can find a match for a spectrum from the database itself.", 'test.msmsSearch.self.match', conn = conn)
	test.that('MSMS search works for an empty spectrum.', 'test.msmsSearch.empty.spectrum', conn = conn)
	test.that('MSMS search works for a null spectrum.', 'test.msmsSearch.null.spectrum', conn = conn)
	test.that('No failure occurs when msmsSearch found no IDs.', 'test.msmsSearch.no.ids', conn = conn)
}
