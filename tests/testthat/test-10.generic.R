# vi: fdm=marker

source('common.R')

# Constants {{{1
################################################################

.MASSFILEDB.URL <- file.path(RES.DIR, 'mass.csv.file.tsv')

# Load reference entries {{{1
################################################################

load.ref.entries <- function(db) {

	# Define reference file
	entries.file <- file.path(RES.DIR, paste0(db, '-entries.txt'))
	expect_true(file.exists(entries.file), info = paste0("Cannot find file \"", entries.file, "\"."))

	# Load reference contents from file
	entries.desc <- read.table(entries.file, stringsAsFactors = FALSE, header = TRUE)
	expect_true(nrow(entries.desc) > 0, info = paste0("No reference entries found in file \"", entries.file, "\" in test.entry.fields()."))

	return(entries.desc)
}

# Test entry fields {{{1
################################################################

test.entry.fields <- function(factory, db) {

	# Load reference entries
	entries.desc <- load.ref.entries(db)

	# Create entries
	entries <- factory$getEntry(db, id = entries.desc[[BIODB.ACCESSION]], drop = FALSE)
	expect_false(any(vapply(entries, is.null, FUN.VALUE = TRUE)), "One of the entries is NULL.")
	expect_equal(length(entries), nrow(entries.desc), info = paste0("Error while retrieving entries. ", length(entries), " entrie(s) obtained instead of ", nrow(entries.desc), "."))

	# Get data frame
	entries.df <- factory$getBiodb()$entriesToDataframe(entries)
	expect_equal(nrow(entries.df), length(entries), info = paste0("Error while converting entries into a data frame. Wrong number of rows: ", nrow(entries.df), " instead of ", length(entries), "."))

	# Test fields of entries
	for (f in colnames(entries.desc)) {
		entries.desc[[f]] <- as.vector(entries.desc[[f]], mode = entries[[1]]$getFieldClass(f))
		e.values <- factory$getBiodb()$entriesFieldToVctOrLst(entries, f, flatten = TRUE)
		expect_equal(e.values, entries.desc[[f]], info = paste0("Error with field \"", f, "\" in entry objects."))
		if (f %in% names(entries.df))
			expect_equal(entries.df[[f]], entries.desc[[f]], info = paste0("Error with field \"", f, "\" in entries data frame"))
		else
			expect_true(all(is.na(entries.desc[[f]])), info = paste("Cannot find field \"", f, "\" in \"entries.df\".", sep = ''))
	}
	
	# Print message about fields that were not tested
	not.tested.fields <- character(0)
	for (e in entries) {
		e.fields <- e$getFieldNames()
		not.tested.fields <- c(not.tested.fields, e.fields[ ! e.fields %in% colnames(entries.desc)])
	}
	if (length(not.tested.fields) > 0) {
		not.tested.fields <- not.tested.fields[ ! duplicated(not.tested.fields)]
		not.tested.fields <- sort(not.tested.fields)
		biodb$message(MSG.CAUTION, paste("Fields \"", paste(not.tested.fields, collapse = ", "), "\" are not tested in database ", db, ".", sep = ''))
	}
}

# TEST WRONG ENTRY {{{1
################################################################

test.wrong.entry <- function(factory, db) {

	# Test a wrong accession number
	wrong.entry <- factory$getEntry(db, id = 'WRONGA')
	expect_null(wrong.entry)
}

# TEST WRONG ENTRY AMONG GOOD ONES {{{1
################################################################

test.wrong.entry.among.good.ones <- function(factory, db) {

	# Load reference entries
	entries.desc <- load.ref.entries(db)

	# Test a wrong accession number
	entries <- factory$getEntry(db, id = c('WRONGB', entries.desc[[BIODB.ACCESSION]]))
	expect_equal(length(entries), nrow(entries.desc) + 1, info = paste0("Error while retrieving entries. ", length(entries), " entrie(s) obtained instead of ", nrow(entries.desc) + 1, "."))
	expect_null(entries[[1]])
	expect_false(any(vapply(entries[2:length(entries)], is.null, FUN.VALUE = TRUE)))
}

# TEST NB ENTRIES {{{1
################################################################

test.nb.entries <- function(db) {

	# Test getNbEntries()
	n <- db$getNbEntries()
	expect_true(is.na(n) || n >= 0)
}

# TEST ENTRY IDS {{{1
################################################################

test.entry.ids <- function(db) {

	# Test getEntryIds()
	max <- 100
	ids <- db$getEntryIds(max.results = max)
	n <- length(ids)
	expect_true(n >= 0 && n <= max)
}

# MAIN {{{1
################################################################

# Create biodb instance
biodb <- create.biodb.instance()

# Get factory
factory <- biodb$getFactory()

# Initialize MassCsvFile
if (BIODB.MASS.CSV.FILE %in% TEST.DATABASES) {
	db.instance <- factory$createConn(BIODB.MASS.CSV.FILE, url = .MASSFILEDB.URL)
	db.instance$setField(BIODB.ACCESSION, c('molid', 'mode', 'col'))
	db.instance$setField(BIODB.COMPOUND.ID, 'molid')
	db.instance$setField(BIODB.MSMODE, 'mode')
	db.instance$setField(BIODB.PEAK.MZTHEO, 'mztheo')
	db.instance$setField(BIODB.PEAK.COMP, 'comp')
	db.instance$setField(BIODB.PEAK.ATTR, 'attr')
	db.instance$setField(BIODB.CHROM.COL, 'col')
	db.instance$setField(BIODB.CHROM.COL.RT, 'colrt')
	db.instance$setField(BIODB.FORMULA, 'molcomp')
	db.instance$setField(BIODB.MASS, 'molmass')
	db.instance$setField(BIODB.FULLNAMES, 'molnames')
	db.instance$setMsMode(BIODB.MSMODE.NEG, 'NEG')
	db.instance$setMsMode(BIODB.MSMODE.POS, 'POS')
}

# Loop on test modes
for (mode in TEST.MODES) {

	# Configure mode
	set.mode(biodb, mode)

	# Loop on test databases
	for (db in TEST.DATABASES) {
		set.test.context(biodb, paste("Running generic tests on", db, "in", mode, "mode"))
		test_that("Wrong entry gives NULL", test.wrong.entry(factory, db))
		test_that("One wrong entry does not block the retrieval of good ones", test.wrong.entry.among.good.ones(factory, db))
		test_that("Entry fields have a correct value", test.entry.fields(factory, db))
		if ( ! is(factory$getConn(db), 'RemotedbConn') || mode == MODE.ONLINE || mode == MODE.QUICK.ONLINE) {
			test_that("Nb entries is positive", test.nb.entries(factory$getConn(db)))
			test_that("We can get a list of entry ids", test.entry.ids(factory$getConn(db)))
		}
	}
}
