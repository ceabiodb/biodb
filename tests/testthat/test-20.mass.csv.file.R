# vi: fdm=marker

source('common.R')

# Offline test mass.csv.file {{{1
################################################################

offline.test.mass.csv.file <- function(biodb) {

	# Open file
	df <- read.table(MASSFILEDB.URL, sep = "\t", header = TRUE, quote = '"', stringsAsFactors = FALSE, row.names = NULL)

	# Create biodb instance
	biodb$getCache()$disable()
	factory <- biodb$getFactory()

	# Create database
	init.mass.csv.file.db(biodb)
	db <- factory$getConn(BIODB.MASS.CSV.FILE)

	# Test number of entries
	expect_gt(db$getNbEntries(), 1)
	expect_equal(db$getNbEntries(), sum( ! duplicated(df[c('molid', 'mode', 'col')])))

	# Get a compound ID
	compound.id <- df[df[['ms.level']] == 1, 'molid'][[1]]

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

# MAIN {{{1
################################################################

if (BIODB.MASS.CSV.FILE %in% TEST.DATABASES && MODE.OFFLINE %in% TEST.MODES) {
	biodb <- create.biodb.instance()
	set.test.context(biodb, "Testing mass.csv.file")
	set.mode(biodb, MODE.OFFLINE)
	test_that("MassCsvFileConn methods are correct", offline.test.mass.csv.file(biodb))
}
