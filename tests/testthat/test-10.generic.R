# vi: fdm=marker

source('common.R')

# Constants {{{1
################################################################

.MASSFILEDB.URL <- file.path(RES.DIR, 'mass.csv.file.tsv')

# Save entries as JSON {{{1
################################################################

save.entries.as.json <- function(db, entries) {

	# Loop on all entries
	for (e in entries) {
		json <- e$getFieldsAsJson()
		writeChar(json, file.path(OUTPUT.DIR, paste(db, '-entry-', e$getFieldValue(BIODB.ACCESSION), '.json', sep = '')))
	}
}

# Test entry fields {{{1
################################################################

test.entry.fields <- function(biodb, db) {

	# Load reference entries
	entries.desc <- load.ref.entries(db)

	# Create entries
	entries <- biodb$getFactory()$getEntry(db, id = entries.desc[[BIODB.ACCESSION]], drop = FALSE)
	expect_false(any(vapply(entries, is.null, FUN.VALUE = TRUE)), "One of the entries is NULL.")
	expect_equal(length(entries), nrow(entries.desc), info = paste0("Error while retrieving entries. ", length(entries), " entrie(s) obtained instead of ", nrow(entries.desc), "."))
	save.entries.as.json(db, entries)

	# Get data frame
	entries.df <- biodb$entriesToDataframe(entries)
	expect_equal(nrow(entries.df), length(entries), info = paste0("Error while converting entries into a data frame. Wrong number of rows: ", nrow(entries.df), " instead of ", length(entries), "."))

	# Test fields of entries
	for (f in colnames(entries.desc)) {
		entries.desc[[f]] <- as.vector(entries.desc[[f]], mode = biodb$getEntryFields()$get(f)$getClass())
		e.values <- biodb$entriesFieldToVctOrLst(entries, f, flatten = TRUE)
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

test.wrong.entry <- function(biodb, db) {

	# Test a wrong accession number
	wrong.entry <- biodb$getFactory()$getEntry(db, id = 'WRONGA')
	expect_null(wrong.entry)
}

# TEST WRONG ENTRY AMONG GOOD ONES {{{1
################################################################

test.wrong.entry.among.good.ones <- function(biodb, db) {

	# Load reference entries
	entries.desc <- load.ref.entries(db)

	# Test a wrong accession number
	entries <- biodb$getFactory()$getEntry(db, id = c('WRONGB', entries.desc[[BIODB.ACCESSION]]))
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

# Test entry ids {{{1
################################################################

test.entry.ids <- function(db) {

	# Test getEntryIds()
	max <- 100
	ids <- db$getEntryIds(max.results = max)
	expect_true(is.character(ids))
	n <- length(ids)
	expect_true(n >= 0 && n <= max)
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

# Test searchPeak() {{{1
################################################################

test.searchMzTol <- function(db) {

	# Get M/Z values from database
	mode <- BIODB.MSMODE.POS
	mz <- db$getMzValues(ms.mode = mode, max.results = 10)
	expect_true(is.double(mz))
	expect_true(length(mz) >= 1)

	# Search
	ids <- db$searchMzTol(mz = mz, tol = 5, tol.unit = BIODB.MZTOLUNIT.PLAIN, min.rel.int = 0, ms.mode = mode)
	print('* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * IDS')
	print(ids)
	expect_true(is.character(ids))
	expect_true(length(ids) > 0)
}

# MAIN {{{1
################################################################

# Create biodb instance
biodb <- create.biodb.instance()

# Initialize MassCsvFile
if (BIODB.MASS.CSV.FILE %in% TEST.DATABASES) {
	db.instance <- biodb$getFactory()$createConn(BIODB.MASS.CSV.FILE, url = .MASSFILEDB.URL)
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
	for (db.name in TEST.DATABASES) {

		set.test.context(biodb, paste("Running generic tests on", db.name, "in", mode, "mode"))
		test_that("Wrong entry gives NULL", test.wrong.entry(biodb, db.name))
		test_that("One wrong entry does not block the retrieval of good ones", test.wrong.entry.among.good.ones(biodb, db.name))
		test_that("Entry fields have a correct value", test.entry.fields(biodb, db.name))

		# Get instance
		db <- biodb$getFactory()$getConn(db.name)

		if ( ! methods::is(db, 'RemotedbConn') || mode == MODE.ONLINE || mode == MODE.QUICK.ONLINE) {
			test_that("Nb entries is positive", test.nb.entries(db))
			test_that("We can get a list of entry ids", test.entry.ids(db))

			# Mass database testing
			if (methods::is(db, 'MassdbConn')) {
				test_that("We can retrieve a list of M/Z values", test.getMzValues(db))
				test_that("We can match M/Z peaks", test.searchMzTol(db))
			}
		}
	}
}
