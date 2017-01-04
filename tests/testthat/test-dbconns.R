# vi: fdm=marker

# CONSTANTS {{{1
################################################################

.MASSFILEDB.URL <- file.path(SCRIPT.DIR, 'tests', 'res', 'massfiledb.tsv')

# TEST ENTRY FIELDS {{{1
################################################################

test_entry_fields <- function(factory, db) {

	# Download contents
	entries.file <- file.path(SCRIPT.DIR, 'tests', 'res', paste0(db, '-entries.txt'))
	entries.desc <- read.table(entries.file, stringsAsFactors = FALSE, header = TRUE)

	# Create entries
	entries <- factory$createEntry(db, id = entries.desc[[BIODB.ACCESSION]], drop = FALSE)

	# Test fields of entries
	for (f in colnames(entries.desc)) {
		entries.desc[[f]] <- as.vector(entries.desc[[f]], mode = entries[[1]]$getFieldClass(f))
		card <- entries[[1]]$getFieldCardinality(f)
		e.values <- vapply(entries, function(e) if (card == BIODB.CARD.ONE || is.na(e$getField(f))) e$getField(f) else paste(e$getField(f), collapse = ';'), FUN.VALUE = vector(mode = entries[[1]]$getFieldClass(f), length = 1))
		expect_equal(e.values, entries.desc[[f]], info = paste0("Error with field \"", f, "\""))
	}
}

# TEST WRONG ENTRY {{{1
################################################################

test_wrong_entry <- function(factory, db) {

	# Test a wrong accession number
	wrong.entry <- factory$createEntry(db, id = 'WRONG')
	expect_null(wrong.entry)
}

# TEST NB ENTRIES {{{1
################################################################

test_nb_entries <- function(db) {

	# Test getNbEntries()
	n <- db$getNbEntries()
	expect_true(is.na(n) || n >= 0)
}

# TEST ENTRY IDS {{{1
################################################################

test_entry_ids <- function(db) {

	# Test getEntryIds()
	max <- 100
	n <- db$getEntryIds(max.results = max)
	expect_true(n >= 0 && n <= max)
}

# MAIN {{{1
################################################################

# Set online/offline modes to test
online.modes = logical()
#if ( ! is.null(opt[['offline']]))
	online.modes <- c(online.modes, FALSE)
#if ( ! is.null(opt[['online']]))
	online.modes <- c(online.modes, TRUE)

biodb <- Biodb$new(useragent = USER.AGENT, use.env.var = TRUE)
biodb$addObservers(BiodbLogger$new()) #file = file.path(SCRIPT.DIR, 'tests', 'test-dbconns.log')))

# Loop on online/offline modes
for (online in online.modes) {

	# Create factory
	# TODO Add option in factory for blocking online access when in offline mode
	factory <- biodb$getFactory()
	factory$setCacheDir(file.path(SCRIPT.DIR, 'tests', if (online) 'cache' else file.path('res', 'offline-files')))
	factory$setCacheMode(if (online) BIODB.CACHE.WRITE.ONLY else BIODB.CACHE.READ.ONLY)

	# Loop on all databases
	for (db in BIODB.DATABASES) {

		# Initialize massfiledb
		# if (db == BIODB.MASSFILEDB) {
			# db.instance <- factory$createConn(db, url = .MASSFILEDB.URL)
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
		# }

#		if (is.null(opt[['databases']]) || db %in% opt[['databases']]) {
			context(paste0("Testing database ", db, if (online) " online" else " offline"))
			test_that("Wrong entry gives NULL", test_wrong_entry(factory, db))
			test_that("Entry fields have a correct value", test_entry_fields(factory, db))
			test_that("Nb entries is positive", test_nb_entries(factory$getConn(db)))
			test_that("We can get a list of entry ids", test_entry_ids(factory$getConn(db)))
#		}
	}
}
