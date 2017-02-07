# vi: fdm=marker

# Offline test massfiledb {{{1
################################################################

offline.test.massfiledb <- function() {

	# Open file
	file <- file.path(SCRIPT.DIR, 'tests', 'res', 'massfiledb.tsv')
	df <- read.table(file, sep = "\t", header = TRUE)

	# Create biodb instance
	biodb <- Biodb$new(logger = FALSE)
	biodb$addObservers(BiodbLogger$new(file = LOG.FILE))
	biodb$getCache()$disable()
	factory <- biodb$getFactory()

	# Create database
	db <- factory$createConn(BIODB.MASSFILEDB, url = file)
	fields <- list()
	db$setField(BIODB.ACCESSION, c('molid', 'mode', 'col'))
	db$setField(BIODB.COMPOUND.ID, 'molid')
	db$setField(BIODB.MSMODE, 'mode')
	db$setField(BIODB.PEAK.MZ, 'mztheo')
	db$setField(BIODB.PEAK.COMP, 'comp')
	db$setField(BIODB.PEAK.ATTR, 'attr')
	db$setField(BIODB.CHROM.COL, 'col')
	db$setField(BIODB.CHROM.COL.RT, 'colrt')
	db$setField(BIODB.FORMULA, 'molcomp')
	db$setField(BIODB.MASS, 'molmass')
	db$setField(BIODB.FULLNAMES, 'molnames')
	db$setMsMode(BIODB.MSMODE.NEG, 'NEG')
	db$setMsMode(BIODB.MSMODE.POS, 'POS')

	# Run general tests
#	test.db(db)

	# Generic tests

		# Test entries
		expect_gt(db$getNbEntries(), 1)

		# Test chrom cols
		entry.id <- db$getEntryIds()[[1]]
		expect_gt(nrow(db$getChromCol()), 1)
		expect_gt(nrow(db$getChromCol(entry.id)), 1)
		expect_lt(nrow(db$getChromCol(entry.id)), nrow(db$getChromCol()))
		expect_true(all(db$getChromCol(entry.id)[[BIODB.ID]] %in% db$getChromCol()[[BIODB.ID]]))

		# Test mz values
		expect_true(is.vector(db$getMzValues()))
		expect_gt(length(db$getMzValues()), 1)
		expect_error(db$getMzValues('wrong.mode.value'), silent = TRUE)
		expect_gt(length(db$getMzValues(BIODB.MSMODE.NEG)), 1)
		expect_gt(length(db$getMzValues(BIODB.MSMODE.POS)), 1)

	# Specific tests for filedb
		# Test entry ids
		expect_equal(db$getEntryIds(), sort(as.character(df[! duplicated(df['molid']), 'molid'])))

		# Test nb entries
		expect_equal(db$getNbEntries(), sum(as.integer(! duplicated(df['molid']))))
}

# MAIN {{{1
################################################################

if ((length(TEST.DATABASES) == 0 || BIODB.MASSFILEDB %in% TEST.DATABASES) && OFFLINE %in% TEST.MODES) {
	context("Testing massfiledb offline")
	test_that("MassfiledbConn methods are correct", offline.test.massfiledb())
}
