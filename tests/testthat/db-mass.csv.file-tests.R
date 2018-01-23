# vi: fdm=marker

# Test basic mass.csv.file {{{1
################################################################

test.basic.mass.csv.file <- function(db) {

	# Open file
	df <- read.table(db$getBaseUrl(), sep = "\t", header = TRUE, quote = '"', stringsAsFactors = FALSE, row.names = NULL)

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
	db.df <- read.table(db$getBaseUrl(), sep = "\t", header = TRUE, quote = '"', stringsAsFactors = FALSE, row.names = NULL)

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

# Test getField {{{1
################################################################

test.getField <- function(db) {
	col.name <- db$getField(tag = 'ms.mode')
	expect_is(col.name, 'character')
	expect_true(nchar(col.name) > 0)
}

# Test setField {{{1
################################################################

test.undefined.fields <- function(db) {
	expect_error(db$setField(tag = 'invalid.tag.name', colname = 'something'), regexp = '^.* Database field tag "invalid.tag.name" is not valid.$')
	expect_error(db$setField(tag = 'ms.mode', colname = 'wrong.col.name'), regexp = '^.* One or more columns among wrong.col.name are not defined in database file.$')
}

# Test undefined fields {{{1
################################################################

test.undefined.fields <- function(db) {
	new.biodb <- create.biodb.instance()
	conn <- new.biodb$getFactory()$createConn(db$getId(), url = db$getBaseUrl())
	chrom.cols <- conn$getChromCol()
}

# Run Mass CSV File tests {{{1
################################################################

run.mass.csv.file.tests <- function(db, mode) {
	run.db.test('Test that setField() method works correctly.', 'test.setField', db)
	run.db.test('Test that getField() method works correctly.', 'test.getField', db)
	run.db.test('Test that we detect undefined fields', 'test.undefined.fields', db)
	run.db.test("MassCsvFileConn methods are correct", 'test.basic.mass.csv.file', db)
	run.db.test("M/Z match output contains all columns of database.", 'test.mass.csv.file.output.columns', db)
}
