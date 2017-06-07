# vi: fdm=marker

# Test msmsSearch {{{1
################################################################

test.msmsSearch <- function(biodb, db.name) {

	db <- biodb$getFactory()$getConn(db.name)

	# Loop on modes
	for (mode in BIODB.MSMODE.VALS) {

		# Get one mz value
		mz <- db$getMzValues(ms.mode = mode, ms.level = 2, max.results = 1, precursor = TRUE)

		# Found corresponding spectrum
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
	tol.ppm <- 5

	all.ids <- db$getEntryIds()
	all.entries <- biodb$getFactory()$getEntry(db.name, all.ids)

	# Loop on levels
	for (ms.level in c(1, 2)) {

		# Get an M/Z value of a precursor
		mz <- db$getMzValues(precursor = TRUE, max.results = 1, ms.level = ms.level)
		expect_length(mz, 1)
		expect_false(is.na(mz))

		# Search for it
		spectra.ids <- db$searchMzTol(mz = mz, tol = tol.ppm, tol.unit = BIODB.MZTOLUNIT.PPM, precursor = TRUE, ms.level = ms.level)
		expect_gte(length(spectra.ids), 1)
		expect_false(any(is.na(spectra.ids)))

		# Get first entry
		for (spectra.id in spectra.ids) {
			entry <- biodb$getFactory()$getEntry(db.name, spectra.id)
			expect_false(is.null(entry))
			expect_false(is.na(entry$getFieldValue(BIODB.MS.LEVEL)))
			expect_equal(entry$getFieldValue(BIODB.MS.LEVEL), ms.level)
			peaks <- entry$getFieldValue(BIODB.PEAKS)
			expect_false(is.null(peaks))
			expect_true(is.data.frame(peaks))
			expect_gt(nrow(peaks), 0)
			expect_true(BIODB.PEAK.MZ %in% colnames(peaks))
			expect_true(BIODB.PEAK.RELATIVE.INTENSITY %in% colnames(peaks))

			# Sort peaks table in decreasing intensity order
			peaks <- peaks[order(peaks[[BIODB.PEAK.RELATIVE.INTENSITY]], decreasing = TRUE), ]

			# Check that precursor peak was matched (the one with highest intensity)
			expect_lt(abs(mz - peaks[1, BIODB.PEAK.MZ]), mz * tol.ppm * 1e-6)
			expect_equal(peaks[1, BIODB.PEAK.RELATIVE.INTENSITY], 100)
		}
	}
}

# Test basic mass.csv.file {{{1
################################################################

test.basic.mass.csv.file <- function(biodb) {

	# Open file
	df <- read.table(MASSFILEDB.URL, sep = "\t", header = TRUE, quote = '"', stringsAsFactors = FALSE, row.names = NULL)

	# Get database
	db <- biodb$getFactory()$getConn(BIODB.MASS.CSV.FILE)

	# Test number of entries
	expect_gt(db$getNbEntries(), 1)
	expect_equal(db$getNbEntries(), sum( ! duplicated(df[c('compoundid', 'msmode', 'chromcol', 'chromcolrt')])))

	# Get a compound ID
	compound.id <- df[df[['ms.level']] == 1, 'compoundid'][[1]]

	# Test number of peaks
	expect_gt(db$getNbPeaks(), 1)
	expect_gt(db$getNbPeaks(mode = BIODB.MSMODE.NEG), 1)
	expect_gt(db$getNbPeaks(mode = BIODB.MSMODE.POS), 1)
	expect_equal(db$getNbPeaks(), nrow(df))
	expect_gt(db$getNbPeaks(compound.ids = compound.id), 1)

	# Test chrom cols
	expect_gt(nrow(db$getChromCol()), 1)
	expect_gt(nrow(db$getChromCol(compound.ids = compound.id)), 1)
	expect_lte(nrow(db$getChromCol(compound.ids = compound.id)), nrow(db$getChromCol()))
	expect_true(all(db$getChromCol(compound.ids = compound.id)[[BIODB.ID]] %in% db$getChromCol()[[BIODB.ID]]))

	# Test mz values
	expect_true(is.vector(db$getMzValues()))
	expect_gt(length(db$getMzValues()), 1)
	expect_error(db$getMzValues('wrong.mode.value'), silent = TRUE)
	expect_gt(length(db$getMzValues(BIODB.MSMODE.NEG)), 1)
	expect_gt(length(db$getMzValues(BIODB.MSMODE.POS)), 1)
}

# Test output columns {{{1
################################################################

test.mass.csv.file.output.columns <- function(biodb) {

	# Open database file
	db.df <- read.table(MASSFILEDB.URL, sep = "\t", header = TRUE, quote = '"', stringsAsFactors = FALSE, row.names = NULL)

	# Get database
	db <- biodb$getFactory()$getConn(BIODB.MASS.CSV.FILE)

	# Get M/Z value
	mz <- db$getMzValues(max.results = 1, ms.level = 1)
	expect_equal(length(mz), 1)

	# Run a match
	spectra.ids <- db$searchMzTol(mz, ms.level = 1, tol = 5, tol.unit = BIODB.MZTOLUNIT.PPM)

	# Get data frame of results
	entries <- biodb$getFactory()$getEntry(BIODB.MASS.CSV.FILE, spectra.ids)
	entries.df <- biodb$entriesToDataframe(entries, only.atomic = FALSE)

	# Check that all columns of database file are found in entries data frame
	# NOTE this supposes that the columns of the database file are named according to biodb conventions.
	expect_true(all(colnames(db.df) %in% colnames(entries.df)), paste("Columns ", paste(colnames(db.df)[! colnames(db.df) %in% colnames(entries.df)], collapse = ', '), " are not included in output.", sep = ''))
}

