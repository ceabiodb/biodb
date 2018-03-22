# vi: fdm=marker

# Test basic mass.csv.file {{{1
################################################################

test.basic.mass.csv.file <- function(db) {

	# Open file
	df <- read.table(db$getBaseUrl(), sep = "\t", header = TRUE, quote = '"', stringsAsFactors = FALSE, row.names = NULL)

	# Test number of entries
	expect_gt(db$getNbEntries(), 1)
	expect_equal(db$getNbEntries(), sum( ! duplicated(df[c('compound.id', 'ms.mode', 'chrom.col.name', 'chrom.rt')])))

	# Get an entry ID
	id <- df[df[['ms.level']] == 1, 'accession'][[1]]

	# Test number of peaks
	expect_gt(db$getNbPeaks(), 1)
	expect_gt(db$getNbPeaks(mode = BIODB.MSMODE.NEG), 1)
	expect_gt(db$getNbPeaks(mode = BIODB.MSMODE.POS), 1)
	expect_equal(db$getNbPeaks(), nrow(df))
	expect_gt(db$getNbPeaks(ids = id), 1)

	# Test chrom cols
	expect_gt(nrow(db$getChromCol()), 1)
	expect_gt(nrow(db$getChromCol(ids = id)), 1)
	expect_lte(nrow(db$getChromCol(ids = id)), nrow(db$getChromCol()))
	expect_true(all(db$getChromCol(ids = id)[['id']] %in% db$getChromCol()[['id']]))

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


# Test setting database with a data frame {{{1
################################################################

test.mass.csv.file.data.frame <- function(db) {

	# Define database data frame
	ids <- 'ZAP'
	mzs <- 12
	df <- data.frame(accession = ids, mz = mzs, mode = '+')

	# New biodb instance
	new.biodb <- biodb::Biodb$new(logger = FALSE)
	conn <- new.biodb$getFactory()$createConn('mass.csv.file')

	# Set database
	conn$setDb(df)

	# Set fields
	conn$setField('accession', 'accession')
	conn$setField('ms.mode', 'mode')
	conn$setField('peak.mztheo', 'mz')

	# Get M/Z values
	expect_identical(mzs, conn$getMzValues())
	expect_identical(ids, conn$getEntryIds())
}

# Test old col names {{{1
################################################################

test.mass.csv.file.old.col.names <- function(db) {

	# Define data frame
	df <- data.frame(compoundid = 'A10', msmode = 'POS', mztheo = 112.07569, peakcomp = 'P9Z6W410', peakattr = '[(M+H)-(H2O)-(NH3)]+', chromcol = 'colAA', chromcolrt = 94.8, compoundcomp = 'J114L6M62O2', compoundmass = 146.10553, fullnames = 'Blablaine')

	# New biodb instance
	new.biodb <- biodb::Biodb$new(logger = FALSE)
	conn <- new.biodb$getFactory()$createConn('mass.csv.file')

	# Set database
	conn$setDb(df)

	# Get chrom cols
	cols <- conn$getChromCol()
	expect_is(cols, "data.frame")
}

# Test fields {{{1
################################################################

test.fields <- function(db) {

	# Create new connector to same file db
	biodb.2 <- biodb::Biodb$new(logger = FALSE)
	db2 <- init.mass.csv.file.db(biodb.2)

	# Get fields
	col.name <- db2$getField('ms.mode')
	expect_is(col.name, 'character')
	expect_true(nchar(col.name) > 0)

	# Test if has field
	expect_true(db2$hasField('accession'))
	expect_false(db2$hasField('blabla'))

	# Add new field
	db2$addField('blabla', 1)

	# Set wrong fields
	expect_error(db2$setField(tag = 'invalid.tag.name', colname = 'something'), regexp = '^.* Database field "invalid.tag.name" is not valid.$')
	expect_error(db2$setField(tag = 'ms.mode', colname = 'wrong.col.name'), regexp = '^.* Column.* is/are not defined in database file.$')

	# Ignore if column name is not found in file
	db2$setField(tag = 'ms.mode', colname = 'wrong.col.name', ignore.if.missing = TRUE)

	# Try to set accession field
	db2$setField('accession', 'compound.id')
}

# Test undefined fields {{{1
################################################################

test.undefined.fields <- function(db) {
	new.biodb <- create.biodb.instance()
	conn <- new.biodb$getFactory()$createConn(db$getId(), url = MASSFILEDB.WRONG.HEADER.URL)
	expect_error(conn$getChromCol(), regexp = '^.* Field.* is/are undefined in file database\\.$')
	new.biodb$terminate()
}

# Test wrong nb cols {{{1
################################################################

test.wrong.nb.cols <- function(db) {
	new.biodb <- create.biodb.instance()
	conn <- new.biodb$getFactory()$createConn(db$getId(), url = MASSFILEDB.WRONG.NB.COLS.URL)
	expect_error(ids <- conn$getEntryIds(), regexp = '^line 1 did not have 12 elements$')
	new.biodb$terminate()
}

# Test field card one {{{1
################################################################

test.field.card.one <- function(db) {

	# Define database data frame
	ids <- c('ZAP', 'ZAP')
	mzs <- c(12, 14)
	modes <- c('+', '-')
	df <- data.frame(ids = ids, mz = mzs, mode = modes)

	# Create connector
	new.biodb <- create.biodb.instance()
	conn <- new.biodb$getFactory()$createConn(db$getId())
	conn$setDb(df)

	# Set fields
	conn$setField('accession', 'ids')
	conn$setField('ms.mode', 'mode')
	conn$setField('peak.mztheo', 'mz')
	expect_error(conn$checkDb(), regexp = '^.*You cannot set multiple values .* for enumerated field ms.mode.$')

	new.biodb$terminate()
}

# Test getMzValues() without peak.attr field {{{1
################################################################

test.getMzValues.without.peak.attr <- function(db) {

	df <- rbind(data.frame(),
            	list(accession = 'BML80005', ms.mode = 'pos', ms.level = 1, peak.mztheo = 219.1127765, peak.intensity = 373076,  peak.relative.intensity = 999),
            	stringsAsFactors = FALSE)

	# Create connector
	new.biodb <- create.biodb.instance()
	conn <- new.biodb$getFactory()$createConn(db$getId())
	conn$setDb(df)

	# Get M/Z values
	mzs <- conn$getMzValues(max.results = 10)
	expect_is(mzs, 'numeric')
	expect_length(mzs, 1)

	# Get M/Z values filtering on precursor
	mzs <- conn$getMzValues(max.results = 10, precursor = TRUE, ms.mode = 'pos')
	expect_is(mzs, 'numeric')
	expect_length(mzs, 0)
}

# Run Mass CSV File tests {{{1
################################################################

run.mass.csv.file.tests <- function(db, mode) {
	run.db.test.that('Test fields manipulation works correctly.', 'test.fields', db)
	run.db.test.that('Test that we detect undefined fields', 'test.undefined.fields', db)
	run.db.test.that("MassCsvFileConn methods are correct", 'test.basic.mass.csv.file', db)
	run.db.test.that("M/Z match output contains all columns of database.", 'test.mass.csv.file.output.columns', db)
	run.db.test.that('Setting database with a data frame works.', 'test.mass.csv.file.data.frame', db)
	run.db.test.that('Failure occurs when loading database file with a line containing wrong number of values.', 'test.wrong.nb.cols', db)
	run.db.test.that('Failure occurs when a field with a cardinality of one has several values for the same accession number.', 'test.field.card.one', db)
	run.db.test.that('We can search for precursor M/Z values without peak.attr column defined.', 'test.getMzValues.without.peak.attr', db)
	run.db.test.that('Old column names are still recognized.', 'test.mass.csv.file.old.col.names', db)
}
