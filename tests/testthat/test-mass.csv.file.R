# vi: fdm=marker

source('init.R')

# Offline test mass.csv.file {{{1
################################################################

offline.test.mass.csv.file <- function() {

	# Open file
	file <- file.path(RES.DIR, 'mass.csv.file.tsv')
	df <- read.table(file, sep = "\t", header = TRUE, quote = '"', stringsAsFactors = FALSE, row.names = NULL)

	# Create biodb instance
	biodb <- Biodb$new(logger = FALSE, observers = BiodbLogger$new(file = file.path(LOG.DIR, 'test-mass.csv.file.log')))
	biodb$getCache()$disable()
	factory <- biodb$getFactory()

	# Create database
	db <- factory$createConn(BIODB.MASS.CSV.FILE, url = file)
	fields <- list()
	db$setField(BIODB.ACCESSION, c('molid', 'mode', 'col'))
	db$setField(BIODB.COMPOUND.ID, 'molid')
	db$setField(BIODB.MSMODE, 'mode')
	db$setField(BIODB.PEAK.MZTHEO, 'mztheo')
	db$setField(BIODB.PEAK.COMP, 'comp')
	db$setField(BIODB.PEAK.ATTR, 'attr')
	db$setField(BIODB.CHROM.COL, 'col')
	db$setField(BIODB.CHROM.COL.RT, 'colrt')
	db$setField(BIODB.FORMULA, 'molcomp')
	db$setField(BIODB.MASS, 'molmass')
	db$setField(BIODB.FULLNAMES, 'molnames')
	db$setMsMode(BIODB.MSMODE.NEG, 'NEG')
	db$setMsMode(BIODB.MSMODE.POS, 'POS')

	# Test number of entries
	expect_gt(db$getNbEntries(), 1)
	expect_equal(db$getNbEntries(), sum( ! duplicated(df[c('molid', 'mode', 'col')])))

	# Get a compound ID
	compound.id <- df[['molid']][[1]]

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

if ((length(TEST.DATABASES) == 0 || BIODB.MASS.CSV.FILE %in% TEST.DATABASES) && MODE.OFFLINE %in% TEST.MODES) {
	context("Testing mass.csv.file")
	test_that("MassCsvFileConn methods are correct", offline.test.mass.csv.file())
}
