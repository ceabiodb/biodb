# vi: fdm=marker

# CONSTANTS {{{1
################################################################

.MASSFILEDB.URL <- file.path(SCRIPT.DIR, 'tests', 'res', 'massfiledb.tsv')

# TEST ENTRY FIELDS {{{1
################################################################

test.entry.fields <- function(factory, db) {

	# Define reference file
	entries.file <- file.path(SCRIPT.DIR, 'tests', 'res', paste0(db, '-entries.txt'))
	expect_true(file.exists(entries.file), info = paste0("Cannot find file \"", entries.file, "\"."))

	# Load reference contents from file
	entries.desc <- read.table(entries.file, stringsAsFactors = FALSE, header = TRUE)
	expect_true(nrow(entries.desc) > 0, info = paste0("No reference entries found in file \"", entries.file, "\" in test.entry.fields()."))

	# Create entries
	factory$message(MSG.DEBUG, paste("ENTRY IDS =", paste(entries.desc[[BIODB.ACCESSION]], collapse = " ||| "), "// FIN"))
	entries <- factory$createEntry(db, id = entries.desc[[BIODB.ACCESSION]], drop = FALSE)
	expect_equal(length(entries), nrow(entries.desc), info = paste0("Error while retrieving entries. ", length(entries), " entrie(s) obtained instead of ", nrow(entries.desc), "."))
	factory$message(MSG.DEBUG, paste("ENTRIES CLASSES =", paste(lapply(entries, class), collapse = " ||| "), "// FIN"))

	# Get data frame
	entries.df <- factory$getBiodb()$entriesToDataframe(entries)
	expect_equal(nrow(entries.df), length(entries), info = paste0("Error while converting entries into a data frame. Wrong number of rows: ", nrow(entries.df), " instead of ", length(entries), "."))

	# Test fields of entries
	for (f in colnames(entries.desc)) {
		entries.desc[[f]] <- as.vector(entries.desc[[f]], mode = entries[[1]]$getFieldClass(f))
		e.values <- factory$getBiodb()$entriesFieldToVctOrLst(entries, f, flatten = TRUE)
		expect_equal(e.values, entries.desc[[f]], info = paste0("Error with field \"", f, "\" in entry objects."))
		expect_equal(entries.df[[f]], entries.desc[[f]], info = paste0("Error with field \"", f, "\" in entries data frame"))
	}
}

# TEST WRONG ENTRY {{{1
################################################################

test.wrong.entry <- function(factory, db) {

	# Test a wrong accession number
	wrong.entry <- factory$createEntry(db, id = 'WRONG')
	expect_null(wrong.entry)
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

# Set online/offline modes to test
online.modes = logical()
if (is.null(opt[['disable-offline']]))
	online.modes <- c(online.modes, FALSE)
if (is.null(opt[['disable-online']]))
	online.modes <- c(online.modes, TRUE)

# Set list of databases to test
databases <- if (is.null(opt[['databases']])) BIODB.DATABASES else opt[['databases']]
unknown.dbs <- opt[['databases']][ ! opt[['databases']] %in% BIODB.DATABASES]
if (length(unknown.dbs) > 0) stop(paste("Unknown database(s): ", paste(unknown.dbs, collapse = ", "), ".", sep = ''))

# Create biodb instance
biodb <- Biodb$new(logger = FALSE)
biodb$addObservers(BiodbLogger$new(file = file.path(SCRIPT.DIR, 'tests', 'test-dbconns.log')))

# Loop on online/offline modes
for (online in online.modes) {

	biodb$getConfig()$set(biodb:::CFG.CACHEDIR, file.path(SCRIPT.DIR, 'tests', if (online) 'cache' else file.path('res', 'offline-files')))

	# Create factory
	# TODO Add option in factory for blocking online access when in offline mode
	factory <- biodb$getFactory()
	factory$setCacheMode(if (online) BIODB.CACHE.WRITE.ONLY else BIODB.CACHE.READ.ONLY)

	# Loop on databases
	for (db in databases) {

		# Initialize massfiledb
		if (db == BIODB.MASSFILEDB) {
			db.instance <- factory$createConn(db, url = .MASSFILEDB.URL)
			# db.instance$setField(BIODB.ACCESSION, c('molid', 'mode', 'col'))
			# db.instance$setField(BIODB.COMPOUND.ID, 'molid')
			# db.instance$setField(BIODB.MSMODE, 'mode')
			# db.instance$setField(BIODB.PEAK.MZTHEO, 'mztheo')
			# db.instance$setField(BIODB.PEAK.COMP, 'comp')
			# db.instance$setField(BIODB.PEAK.ATTR, 'attr')
			# db.instance$setField(BIODB.CHROM.COL, 'col')
			# db.instance$setField(BIODB.CHROM.COL.RT, 'colrt')
			# db.instance$setField(BIODB.FORMULA, 'molcomp')
			# db.instance$setField(BIODB.MASS, 'molmass')
			# db.instance$setField(BIODB.FULLNAMES, 'molnames')
			# db.instance$setMsMode(BIODB.MSMODE.NEG, 'NEG')
			# db.instance$setMsMode(BIODB.MSMODE.POS, 'POS')
		}

		context(paste0("Testing database ", db, if (online) " online" else " offline"))
		test_that("Entry fields have a correct value", test.entry.fields(factory, db))
		if (online) {
			test_that("Wrong entry gives NULL", test.wrong.entry(factory, db))
			test_that("Nb entries is positive", test.nb.entries(factory$getConn(db)))
			test_that("We can get a list of entry ids", test.entry.ids(factory$getConn(db)))
		}
	}
}
