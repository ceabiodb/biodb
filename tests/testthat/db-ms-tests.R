# vi: fdm=marker

# Test msmsSearch self match {{{1
################################################################

test.msmsSearch.self.match <- function(db) {

	biodb <- db$getBiodb()
	db.name <- db$getId()

	# Loop on modes
	for (mode in BIODB.MSMODE.VALS) {

		# Get one mz value
		mz <- db$getMzValues(ms.mode = mode, ms.level = 2, max.results = 1, precursor = TRUE)

		# Found corresponding spectrum
		spectrum.id <- db$searchMzTol(mz, mz.tol = 5, mz.tol.unit = BIODB.MZTOLUNIT.PPM, ms.mode = mode, max.results = 1, ms.level = 2)

		# Get entry
		spectrum.entry <- biodb$getFactory()$getEntry(db.name, spectrum.id)

		# Get peaks
		peaks <- spectrum.entry$getFieldValue('PEAKS')

		# Run MSMS search
		result <- db$msmsSearch(peaks, precursor = mz, mz.tol = 0.1, mz.tol.unit = BIODB.MZTOLUNIT.PLAIN, ms.mode = mode, npmin = 2, dist.fun = 'pbachtttarya', msms.mz.tol = 3, msms.mz.tol.min = 0.005)

		# Check results
		expect_true( ! is.null(result))
		expect_true(is.data.frame(result))
		expect_true(nrow(result) > 0)
		cols <- c('id', 'score', paste('peak', seq(nrow(peaks)), sep = '.'))
		expect_true(all(cols %in% colnames(result)))
	}
}

# Test msmsSearch massbank {{{1
################################################################

test.msmsSearch.massbank <- function(db) {

	# Define spectrum to match:
	spectrum <- data.frame(mz = c(100.100, 83.100), rel.int = c(100, 10))

	# Search for match:
	result <- db$msmsSearch(spectrum, precursor.mz = 100, mz.tol = 0.3)

	expect_true( ! is.null(result))
	expect_true(is.data.frame(result))
	expect_true(nrow(result) > 0)
	cols <- c('id', 'score', paste('peak', seq(nrow(spectrum)), sep = '.'))
	expect_true(all(cols %in% colnames(result)))
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
	db.values <- list(massbank.eu = list('1' = list(mz = 313.3), '2' = list(mz = 285.0208)))

	db <- biodb$getFactory()$getConn(db.name)
	tol.ppm <- 5

	# Loop on levels
	for (ms.level in c(1, 2)) {

		# Get an M/Z value of a precursor
		if (db.name %in% names(db.values))
			mz <- db.values[[db.name]][[ms.level]]$mz
		else
			mz <- db$getMzValues(precursor = TRUE, max.results = 1, ms.level = ms.level)
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
			expect_false(is.na(entry$getFieldValue('MS.LEVEL')))
			expect_equal(entry$getFieldValue('MS.LEVEL'), ms.level)
			peaks <- entry$getFieldValue('PEAKS')
			expect_false(is.null(peaks))
			expect_true(is.data.frame(peaks))
			expect_gt(nrow(peaks), 0)
			expect_true('peak.mz' %in% colnames(peaks))

			# Check that precursor peak was matched
			expect_true(any(abs(mz - peaks[['peak.mz']] < mz * tol.ppm * 1e-6)))
			expect_true(entry$hasField('MSPRECMZ'))
			expect_true(abs(entry$getFieldValue('MSPRECMZ') - mz) < mz * tol.ppm * 1e-6)
		}
	}
}

# Test basic mass.csv.file {{{1
################################################################

test.basic.mass.csv.file <- function(db) {

	biodb <- db$getBiodb()

	# Open file
	df <- read.table(MASSFILEDB.URL, sep = "\t", header = TRUE, quote = '"', stringsAsFactors = FALSE, row.names = NULL)

	# Test number of entries
	expect_gt(db$getNbEntries(), 1)
	expect_equal(db$getNbEntries(), sum( ! duplicated(df[c('compound.id', 'ms.mode', 'chrom.col', 'chrom.col.rt')])))

	# Get a compound ID
	compound.id <- df[df[['ms.level']] == 1, 'compound.id'][[1]]

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
	expect_true(all(db$getChromCol(compound.ids = compound.id)[['id']] %in% db$getChromCol()[['id']]))

	# Test mz values
	expect_true(is.vector(db$getMzValues()))
	expect_gt(length(db$getMzValues()), 1)
	expect_error(db$getMzValues('wrong.mode.value'), silent = TRUE)
	expect_gt(length(db$getMzValues(BIODB.MSMODE.NEG)), 1)
	expect_gt(length(db$getMzValues(BIODB.MSMODE.POS)), 1)
}

# Test output columns {{{1
################################################################

test.mass.csv.file.output.columns <- function(db) {

	biodb <- db$getBiodb()

	# Open database file
	db.df <- read.table(MASSFILEDB.URL, sep = "\t", header = TRUE, quote = '"', stringsAsFactors = FALSE, row.names = NULL)

	# Get M/Z value
	mz <- db$getMzValues(max.results = 1, ms.level = 1)
	expect_equal(length(mz), 1)

	# Run a match
	spectra.ids <- db$searchMzTol(mz, ms.level = 1, mz.tol = 5, mz.tol.unit = BIODB.MZTOLUNIT.PPM)

	# Get data frame of results
	entries <- biodb$getFactory()$getEntry(BIODB.MASS.CSV.FILE, spectra.ids)
	entries.df <- biodb$entriesToDataframe(entries, only.atomic = FALSE)

	# Check that all columns of database file are found in entries data frame
	# NOTE this supposes that the columns of the database file are named according to biodb conventions.
	expect_true(all(colnames(db.df) %in% colnames(entries.df)), paste("Columns ", paste(colnames(db.df)[! colnames(db.df) %in% colnames(entries.df)], collapse = ', '), " are not included in output.", sep = ''))
}

# Test peak table {{{1
################################################################

test.peak.table <- function(db) {

	biodb <- db$getBiodb()
	db.name <- db$getId()

	# Load reference entries
	entries.desc <- load.ref.entries(db.name)

	# Create entries
	entries <- biodb$getFactory()$getEntry(db.name, id = entries.desc[['accession']], drop = FALSE)
	expect_false(any(vapply(entries, is.null, FUN.VALUE = TRUE)), "One of the entries is NULL.")
	expect_equal(length(entries), nrow(entries.desc), info = paste0("Error while retrieving entries. ", length(entries), " entrie(s) obtained instead of ", nrow(entries.desc), "."))

	# Check number of peaks
	if ('nbpeaks' %in% colnames(entries.desc)) {

		# Check that the registered number of peaks is correct
		expect_equal(vapply(entries, function(e) e$getFieldValue('nbpeaks'), FUN.VALUE = 10), entries.desc[['nbpeaks']])

		# Check that the peak table has this number of peaks
		peak.tables <- lapply(entries, function(e) e$getFieldValue('peaks'))
		expect_false(any(vapply(peak.tables, is.null, FUN.VALUE = TRUE)))
		expect_equal(entries.desc[['nbpeaks']], vapply(peak.tables, nrow, FUN.VALUE = 1))

		# Check that the peak table contains the right columns
		# TODO
	}
}
