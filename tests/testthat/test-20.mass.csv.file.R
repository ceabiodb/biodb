# vi: fdm=marker

source('common.R')

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

test.output.columns <- function(biodb) {

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

# Main {{{1
################################################################

if (BIODB.MASS.CSV.FILE %in% TEST.DATABASES && MODE.OFFLINE %in% TEST.MODES) {
	biodb <- create.biodb.instance()
	init.mass.csv.file.db(biodb)
	set.test.context(biodb, "Testing mass.csv.file")
	set.mode(biodb, MODE.OFFLINE)
	test_that("MassCsvFileConn methods are correct", test.basic.mass.csv.file(biodb))
	test_that("M/Z match output contains all columns of database.", test.output.columns(biodb))
}
