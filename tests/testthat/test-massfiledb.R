# vi: fdm=marker

##############
# MASSFILEDB #
##############

offline.test.massfiledb <- function() {

	# Open file
	file <- file.path(SCRIPT.DIR, 'tests', 'res', 'massfiledb.tsv')
	df <- read.table(file, sep = "\t", header = TRUE)

	# Create factory
	factory <- BiodbFactory$new()

	# Create database
	db <- factory$getConn(BIODB.MASSFILEDB, url = file)
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
		checkTrue(db$getNbEntries() > 1)

		# Test chrom cols
		entry.id <- db$getEntryIds()[[1]]
		checkTrue(nrow(db$getChromCol()) > 1)
		checkTrue(nrow(db$getChromCol(entry.id)) > 1)
		checkTrue(nrow(db$getChromCol(entry.id)) < nrow(db$getChromCol()))
		checkTrue(all(db$getChromCol(entry.id)[[BIODB.ID]] %in% db$getChromCol()[[BIODB.ID]]))

		# Test mz values
		checkTrue(is.vector(db$getMzValues()))
		checkTrue(length(db$getMzValues()) > 1)
		checkException(db$getMzValues('wrong.mode.value'), silent = TRUE)
		checkTrue(length(db$getMzValues(BIODB.MSMODE.NEG)) > 1)
		checkTrue(length(db$getMzValues(BIODB.MSMODE.POS)) > 1)

	# Specific tests for filedb
		# Test entry ids
		checkEquals(db$getEntryIds(), sort(as.character(df[! duplicated(df['molid']), 'molid'])))

		# Test nb entries
		checkTrue(db$getNbEntries() == sum(as.integer(! duplicated(df['molid']))))
}

#############
# MAIN {{{1 #
#############

context("Testing massfiledb offline")
test_that("MassfiledbConn methods are correct", offline.test.massfiledb())
