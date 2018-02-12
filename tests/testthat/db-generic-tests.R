# vi: fdm=marker

# Test entry fields {{{1
################################################################

test.entry.fields <- function(db) {

	biodb <- db$getBiodb()
	db.name <- db$getId()
	db.id.field <- biodb$getDbsInfo()$get(db.name)$getEntryIdField()

	# Get IDs of reference entries
	ref.ids <- list.ref.entries(db.name)

	# Create entries
	entries <- biodb$getFactory()$getEntry(db.name, id = ref.ids, drop = FALSE)
	expect_equal(length(entries), length(ref.ids), info = paste0("Error while retrieving entries. ", length(entries), " entrie(s) obtained instead of ", length(ref.ids), "."))

	# Compute fields
	biodb$computeFields(entries)

	# Save downloaded entries as JSON
	json.files <- file.path(OUTPUT.DIR, paste(db.name, '-entry-', ref.ids, '.json', sep = ''))
	biodb$saveEntriesAsJson(entries, json.files)

	# Loop on all entries
	entry.fields <- character(0)
	ref.entry.fields <- character(0)
	for (i in seq_along(ref.ids)) {

		# Get ID
		id <- ref.ids[[i]]

		# Get entry
		e <- entries[[i]]
		expect_false(is.null(e), info = paste0('Entry ', id, ' of database ', db.name, ' could not be loaded for testing.'))

		# Check IDs
		expect_true(e$hasField('accession'), info = paste0(db.name, ' entry ', id, ' has no accession number.'))
		expect_true(e$hasField(db.id.field), info = paste0(db.name, ' entry ', id, ' has no field ', db.id.field, '.'))
		expect_equal(id, e$getFieldValue('accession'), info = paste0(db.name, ' entry ', id, ' has an accession number (', e$getFieldValue('accession'), ') different from the ID.'))
		expect_equal(e$getFieldValue('accession'), e$getFieldValue(db.id.field), info = paste0(db.name, ' entry ', id, ' has a value (', e$getFieldValue(db.id.field), ') of database id field (', db.id.field, ') different from the accession number (', e$getFieldValue('accession'), ').'))

		# Load reference entry
		ref.entry <- load.ref.entry(db.name, id)

		# Loop on all reference fields
		for (f in names(ref.entry)) {
			expect_true(e$hasField(f), info = paste0('Field "', f, '" cannot be found inside ', db.name, ' entry ', id, '.'))
			expect_equal(typeof(e$getFieldValue(f)), typeof(ref.entry[[f]]), info = paste0('Type of field "', f, '" for database ', db.name, ' entry ', id, ' (', typeof(e$getFieldValue(f)), ') is different in reference entry (', typeof(ref.entry[[f]]), ').'))
			expect_equal(length(e$getFieldValue(f)), length(ref.entry[[f]]), info = paste0('Length of field "', f, '" for database ', db.name, ' entry ', id, ' (', length(e$getFieldValue(f)), ') is different in reference entry (', length(ref.entry[[f]]), ').'))
			expect_identical(e$getFieldValue(f), ref.entry[[f]], info = paste0('Value of field "', f, '" for database ', db.name, ' entry ', id, ' (', paste(e$getFieldValue(f), collapse = ', '), ') is different in reference entry (', paste(ref.entry[[f]], collapse = ', '), ').'))
		}

		# Loop on all fields of loaded entry
		for (f in e$getFieldNames())
			if ( ! f %in% c(db.id.field, 'peaks'))
				expect_true(any(biodb$getEntryFields()$get(f)$getAllNames() %in% names(ref.entry)), info = paste0('Field ', f, ' of ', db.name, ' entry ', id, ' has not been tested. Its values is: ', paste(e$getFieldValue(f), collapse = ', '), '.'))

		# Store all encountered fields
		entry.fields <- c(entry.fields, e$getFieldNames())
		ref.entry.fields <- c(ref.entry.fields, names(ref.entry))
	}

	# Search for untested fields and send a Biodb CAUTION message
	not.tested.fields <- entry.fields[ ! entry.fields %in% ref.entry.fields]
	not.tested.fields <- not.tested.fields[ ! duplicated(not.tested.fields)]
	for (f in not.tested.fields)
		biodb$message('caution', paste("Field \"", f, "\" of database ", db.name, " is never tested.", sep = ''))
}

# Test wrong entry {{{1
################################################################

test.wrong.entry <- function(db) {

	biodb <- db$getBiodb()
	db.name <- db$getId()

	# Test a wrong accession number
	wrong.entry <- biodb$getFactory()$getEntry(db.name, id = 'WRONGA')
	expect_null(wrong.entry)
}

# Test wrong entry among good ones {{{1
################################################################

test.wrong.entry.among.good.ones <- function(db) {

	biodb <- db$getBiodb()
	db.name <- db$getId()

	# Load reference entries
	entries.desc <- load.ref.entries(db.name)

	# Test a wrong accession number
	entries <- biodb$getFactory()$getEntry(db.name, id = c('WRONGB', entries.desc[['accession']]))
	expect_equal(length(entries), nrow(entries.desc) + 1, info = paste0("Error while retrieving entries. ", length(entries), " entrie(s) obtained instead of ", nrow(entries.desc) + 1, "."))
	expect_null(entries[[1]])
	expect_false(any(vapply(entries[2:length(entries)], is.null, FUN.VALUE = TRUE)))
}

# Test peak table {{{1
################################################################

test.peak.table <- function(db) {

	biodb <- db$getBiodb()
	db.name <- db$getId()

	# Load reference entries
	entries.desc <- load.ref.entries(db.name)

	# Create entries
	entries <- biodb$getFactory()$getEntry(db.name, id = entries.desc[['accession']], drop = FALSE)
	expect_false(any(vapply(entries, is.null, FUN.VALUE = TRUE)), "One of the entries is NULL.")
	expect_equal(length(entries), nrow(entries.desc), info = paste0("Error while retrieving entries. ", length(entries), " entrie(s) obtained instead of ", nrow(entries.desc), "."))

	# Check number of peaks
	if ('nbpeaks' %in% colnames(entries.desc)) {

		# Check that the registered number of peaks is correct
		expect_equal(vapply(entries, function(e) e$getFieldValue('nbpeaks'), FUN.VALUE = 10), entries.desc[['nbpeaks']])

		# Check that the peak table has this number of peaks
		peak.tables <- lapply(entries, function(e) e$getFieldValue('peaks'))
		expect_false(any(vapply(peak.tables, is.null, FUN.VALUE = TRUE)))
		expect_equal(entries.desc[['nbpeaks']], vapply(peak.tables, nrow, FUN.VALUE = 1))

		# Check that the peak table contains the right columns
		# TODO
	}
}

# Test nb entries {{{1
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
	if ( ! is.null(ids)) {
		expect_true(is.character(ids))
		n <- length(ids)
		expect_true(n >= 0 && n <= max)
	}
}

# Test RT unit {{{1
################################################################

test.rt.unit <- function(db) {

	# Get IDs of reference entries
	ref.ids <- list.ref.entries(db$getId())

	# Create entries
	entries <- db$getBiodb()$getFactory()$getEntry(db$getId(), id = ref.ids, drop = FALSE)

	# Loop on all entries
	for (e in entries)
		expect_true( ! e$hasField('chrom.rt') || e$hasField('chrom.rt.unit'), paste('No RT unit for entry ', e$getFieldValue('accession'), '. If an entry defines a retention time, it must also defines the unit.', sep = ''))
}

# Test entry page URL {{{1
################################################################

test.entry.page.url <- function(db) {

	# Get IDs of reference entries
	ref.ids <- list.ref.entries(db$getId())

	# Get URLs
	urls <- db$getEntryPageUrl(ref.ids)

	# Check
	expect_is(urls, 'character')
	expect_length(urls, length(ref.ids))
	expect_false(any(is.na(urls)))
}

# Test entry image URL {{{1
################################################################

test.entry.image.url <- function(db) {

	# Get IDs of reference entries
	ref.ids <- list.ref.entries(db$getId())

	# Get URLs
	urls <- db$getEntryImageUrl(ref.ids)

	# Check
	expect_is(urls, 'character')
	expect_length(urls, length(ref.ids))
}

# Test entry page URL download {{{1
################################################################

test.entry.page.url.download <- function(db) {

	# Get IDs of reference entries
	ref.ids <- list.ref.entries(db$getId())

	# Get URL
	url <- db$getEntryPageUrl(ref.ids[[1]])

	# Try downloading
	content <- RCurl::getURL(url)
	expect_true( ! is.na(content))
	expect_true(nchar(content) > 0)
}

# Test entry image URL download {{{1
################################################################

test.entry.image.url.download <- function(db) {

	# Get IDs of reference entries
	ref.ids <- list.ref.entries(db$getId())

	# Get URL
	url <- db$getEntryImageUrl(ref.ids[[1]])
	expect_is(url, 'character')

	# Try downloading
	if ( ! is.na(url)) {
		content <- RCurl::getBinaryURL(url)
		expect_is(content, 'raw')
	}
}

# Run db generic tests {{{1
################################################################

run.db.generic.tests <- function(db, mode) {

	set.test.context(db$getBiodb(), paste("Running generic tests on database", db$getId(), "in", mode, "mode"))

	run.db.test.that("Wrong entry gives NULL", 'test.wrong.entry', db)
	run.db.test.that("One wrong entry does not block the retrieval of good ones", 'test.wrong.entry.among.good.ones', db)
	run.db.test.that("Entry fields have a correct value", 'test.entry.fields', db)
	run.db.test.that("The peak table is correct.", 'test.peak.table', db)
	run.db.test.that("RT unit is defined when there is an RT value.", 'test.rt.unit', db)
	if ( ! methods::is(db, 'RemotedbConn') || mode %in% c(MODE.ONLINE, MODE.QUICK.ONLINE)) {
		run.db.test.that("Nb entries is positive.", 'test.nb.entries', db)
		run.db.test.that("We can get a list of entry ids.", 'test.entry.ids', db)
	}
	if (methods::is(db, 'RemotedbConn')) {
		run.db.test.that("We can get a URL pointing to the entry page.", 'test.entry.page.url', db)
		run.db.test.that("We can get a URL pointing to the entry image.", 'test.entry.image.url', db)
		if (mode %in% c(MODE.ONLINE, MODE.QUICK.ONLINE)) {
			run.db.test.that("The entry page URL can be downloaded.", 'test.entry.page.url.download', db)
			run.db.test.that("The entry image URL can be downloaded.", 'test.entry.image.url.download', db)
		}
	}
}
