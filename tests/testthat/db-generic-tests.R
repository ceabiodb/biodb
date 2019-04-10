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
	testthat::expect_equal(length(entries), length(ref.ids), info = paste0("Error while retrieving entries. ", length(entries), " entrie(s) obtained instead of ", length(ref.ids), "."))

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
			v = ref.entry[[f]]
			w = e$getFieldValue(f)
			if (is.data.frame(v))
				v = as.data.frame(v, stringsAsFactors = FALSE)

			# Check value
			expect_true(e$hasField(f), info = paste0('Field "', f, '" cannot be found inside ', db.name, ' entry ', id, '.'))
			expect_equal(typeof(w), typeof(v), info = paste0('Type of field "', f, '" for database ', db.name, ' entry ', id, ' (', typeof(w), ') is different in reference entry (', typeof(v), ').'))
			expect_equal(length(w), length(v), info = paste0('Length of field "', f, '" for database ', db.name, ' entry ', id, ' (', length(w), ') is different in reference entry (', length(v), ').'))
			if ( ! is.vector(v) || length(v) < 20 || length(v) != length(w))
				expect_identical(w, v, info = paste0('Value of field "', f, '" for database ', db.name, ' entry ', id, ' (', paste(w, collapse = ', '), ') is different in reference entry (', paste(v, collapse = ', '), ').'))
			else
				expect_identical(w, v, info = paste0('Value of field "', f, '" for database ', db.name, ' entry ', id, ' is different in reference entry. Non equal values are: ', paste(vapply(which(v != w), function(i) paste(w[[i]], '!=', v[[i]]), FUN.VALUE = ''), collapse = ', '), '.'))
		}

		# Loop on all fields of loaded entry
		for (f in e$getFieldNames())
			if ( ! f %in% c(db.id.field, 'peaks'))
				expect_true(any(biodb$getEntryFields()$get(f)$getAllNames() %in% names(ref.entry)), info = paste0('Field ', f, ' of ', db.name, ' entry ', id, ' has not been tested. Its value is: ', paste(e$getFieldValue(f), collapse = ', '), '.'))

		# Store all encountered fields
		entry.fields <- c(entry.fields, e$getFieldNames())
		ref.entry.fields <- c(ref.entry.fields, names(ref.entry))
	}

	# Search for untested fields and send a Biodb CAUTION message
	not.tested.fields <- entry.fields[ ! entry.fields %in% c(ref.entry.fields, db.id.field)]
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
   ids <- c('WRONGB', entries.desc[['accession']])
   entries <- biodb$getFactory()$getEntry(db.name, id = ids)
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
	expect_is(ids, 'character')
	expect_true(length(ids) <= max)
}

# Test RT unit {{{1
################################################################

test.rt.unit <- function(db) {

	# Get IDs of reference entries
	ref.ids <- list.ref.entries(db$getId())

	# Get entries
	entries <- db$getBiodb()$getFactory()$getEntry(db$getId(), id = ref.ids, drop = FALSE)

	# Loop on all entries
	for (e in entries)
		expect_true( ( ! e$hasField('chrom.rt') && ! e$hasField('chrom.rt.min') && ! e$hasField('chrom.rt.max')) || e$hasField('chrom.rt.unit'), paste('No RT unit for entry ', e$getFieldValue('accession'), '. If an entry defines a retention time, it must also defines the unit.', sep = ''))
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
	expect_length(grep('<title>.*Not Found</title>', content), 0)
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

# Test create connector with same URL {{{1
################################################################

test.create.conn.with.same.url = function(conn) {
	testthat::expect_error(conn$getBiodb()$getFactory()$createConn(conn$getDbClass(), url = conn$getUrl('base.url')))
}

# Test database edition {{{1
################################################################

test.db.editing = function(conn) {

	# Get one entry from connector
	id = conn$getEntryIds(1)
	entry = conn$getEntry(id)
	testthat::expect_is(entry, 'BiodbEntry')

	# Create other connector
	conn.2 = conn$getBiodb()$getFactory()$createConn(conn$getDbClass())
	conn.2$allowEditing()
	conn.2$addNewEntry(entry$clone())

	# Test methods
	id = conn.2$getEntryIds()
	testthat::expect_length(id, 1)
	testthat::expect_equal(id, entry$getFieldValue('accession'))

	# Delete connector
	conn.2$getBiodb()$getFactory()$deleteConn(conn.2$getId())
}

# Test database writing with column addition {{{1
################################################################

test.db.writing.with.col.add = function(conn) {

	# Set database file
	db.file <- file.path(OUTPUT.DIR, paste('test.db.writing.with.col.add', conn$getDbClass(), 'db', sep = '.'))
	if (file.exists(db.file))
		unlink(db.file)

	# Get one entry from connector
	id = conn$getEntryIds(1)
	entry = conn$getEntry(id)
	testthat::expect_is(entry, 'BiodbEntry')

	# Create other connector
	conn.2 = conn$getBiodb()$getFactory()$createConn(conn$getDbClass(), url = db.file)
	conn.2$allowEditing()
	conn.2$addNewEntry(entry$clone())
	conn.2$allowWriting()

	# Get data frame of all entries
	id = conn.2$getEntryIds()
	testthat::expect_length(id, 1)
	testthat::expect_equal(id, entry$getFieldValue('accession'))
	entries = conn.2$getEntry(id, drop = FALSE)
	entries.df = conn.2$getBiodb()$entriesToDataframe(entries, compute = FALSE, only.card.one = TRUE)

	# Get a list of all fields currently used in connector
	current.fields = colnames(entries.df)

	# Choose a field that is not used inside the connector
	fields.def = conn.2$getBiodb()$getEntryFields()
	for (field.name in fields.def$getFieldNames()) {
		field = fields.def$get(field.name)
		if (field$hasCardOne() && field$isVector() && ! field.name %in% current.fields)
			break
	}

	# Create a new entry having one new field that does not exist in any other entry (so in the case of SQL the table will have to be altered to add new columns)
	new.entry = entries[[1]]$clone()
	new.entry$setFieldValue('accession', 'anewentry')
	new.entry$setFieldValue(field$getName(), 0)

	# Add and write the new entry
	conn.2$addNewEntry(new.entry)
	conn.2$write()
	conn.2$getBiodb()$getFactory()$deleteConn(conn.2$getId())
}

# Test database writing {{{1
################################################################

test.db.writing = function(conn) {

	biodb = conn$getBiodb()

	# Get one entry
	entry = conn$getEntry(conn$getEntryIds(1))

	# Set database file
	db.file <- file.path(OUTPUT.DIR, paste('test.db.writing', conn$getDbClass(), 'db', sep = '.'))
	if (file.exists(db.file))
		unlink(db.file)

	# Create new database
	conn.2 = biodb$getFactory()$createConn(conn$getDbClass())
	testthat::expect_error(conn.2$write())

	# Clone entry
	testthat::expect_true(entry$parentIsAConnector())
	entry.2 = entry$clone()
	testthat::expect_false(entry.2$parentIsAConnector())

	# Compare entries
	df.1 <- biodb$entriesToDataframe(list(entry), only.atomic = FALSE, sort.cols = TRUE)
	df.2 <- biodb$entriesToDataframe(list(entry.2), only.atomic = FALSE, sort.cols = TRUE)
	testthat::expect_identical(df.1, df.2)

	# Add new entry
	testthat::expect_length(conn.2$getAllCacheEntries(), 0)
	testthat::expect_null(conn.2$getEntry(entry$getId()))
	testthat::expect_length(conn.2$getAllCacheEntries(), 0)
	testthat::expect_error(conn.2$addNewEntry(entry.2))
	testthat::expect_length(conn.2$getAllCacheEntries(), 0)
	conn.2$allowEditing()
	conn.2$addNewEntry(entry.2)
	testthat::expect_length(conn.2$getAllCacheEntries(), 1)
	testthat::expect_true(entry.2$parentIsAConnector())
	testthat::expect_error(conn.2$addNewEntry(entry.2))
	testthat::expect_length(conn.2$getAllCacheEntries(), 1)

	# Write database
	testthat::expect_true(entry.2$isNew())
	testthat::expect_error(conn.2$write())
	conn.2$setUrl('base.url', db.file)
	testthat::expect_error(conn.2$write())
	conn.2$allowWriting()
	conn.2$write()
	testthat::expect_false(entry.2$isNew())

	# Reload database file
	biodb$getFactory()$deleteConn(conn.2$getId())
	conn.3 = biodb$getFactory()$createConn(conn$getDbClass(), url = db.file)
	entry.3 = conn.3$getEntry(entry$getId())
	testthat::expect_is(entry.3, 'BiodbEntry')
	biodb$getFactory()$deleteConn(conn.3$getId())
}

# Test database copy {{{1
################################################################

test.db.copy = function(conn) {

	biodb = conn$getBiodb()

	# Set database file
	db.file <- file.path(OUTPUT.DIR, paste('test.db.copy', conn$getDbClass(), 'db', sep = '.'))
	if (file.exists(db.file))
		unlink(db.file)

	# Create new connector
	conn.2 = biodb$getFactory()$createConn(conn$getDbClass(), url = db.file)

	# Copy database
	conn.2$allowEditing()
	conn.2$allowWriting()
	biodb$copyDb(conn.from = conn, conn.t = conn.2)

	# Compare
	ids.1 = conn$getEntryIds()
	ids.2 = conn.2$getEntryIds()
	testthat::expect_identical(ids.1, ids.2)
	entries.1 = conn$getEntry(ids.1)
	entries.2 = conn.2$getEntry(ids.2)
	df.1 = biodb$entriesToDataframe(entries.1, only.atomic = FALSE, compute = FALSE)
	df.2 = biodb$entriesToDataframe(entries.2, only.atomic = FALSE, compute = FALSE)
	testthat::expect_identical(df.1, df.2)

	# Delete connector
	biodb$getFactory()$deleteConn(conn.2$getId())
}

# Test searchByName() {{{1
################################################################

test.searchByName = function(conn) {

	# Get an entry
	id <- list.ref.entries(conn$getId())[[1]]
	testthat::expect_true( ! is.null(id))
	testthat::expect_length(id, 1)
	entry <- conn$getEntry(id, drop = TRUE)
	testthat::expect_true( ! is.null(entry))

	# Search by name
	name <- entry$getFieldValue('name')
	testthat::expect_is(name, 'character')
	testthat::expect_gt(length(name), 0)
	name <- name[[1]]
	testthat::expect_true( ! is.na(name))
	ids <- conn$searchByName(name = name)

	# Test
	msg <- paste0('While searching for entry ', id, ' by name "', name, '".')
	testthat::expect_true( ! is.null(ids), msg)
	testthat::expect_true(length(ids) > 0, msg)
	testthat::expect_true(id %in% ids, msg)
}

# Run db generic tests {{{1
################################################################

run.db.generic.tests = function(conn) {

	test.that("Wrong entry gives NULL", 'test.wrong.entry', conn = conn)
	test.that("One wrong entry does not block the retrieval of good ones", 'test.wrong.entry.among.good.ones', conn = conn)
	test.that("Entry fields have a correct value", 'test.entry.fields', conn = conn)
	test.that("The peak table is correct.", 'test.peak.table', conn = conn)
	test.that("RT unit is defined when there is an RT value.", 'test.rt.unit', conn = conn)
	if ( ! conn$isRemotedb() || test.online()) {
		test.that("Nb entries is positive.", 'test.nb.entries', conn = conn)
		test.that("We can get a list of entry ids.", 'test.entry.ids', conn = conn)
		if (conn$isSearchable())
			test.that("We can search for an entry by name.", 'test.searchByName', conn = conn)
	}
	if (conn$isRemotedb()) {
		test.that("We can get a URL pointing to the entry page.", 'test.entry.page.url', conn = conn)
		test.that("We can get a URL pointing to the entry image.", 'test.entry.image.url', conn = conn)
		if (test.online()) {
			test.that("The entry page URL can be downloaded.", 'test.entry.page.url.download', conn = conn)
			test.that("The entry image URL can be downloaded.", 'test.entry.image.url.download', conn = conn)
		}
	}
	if (conn$isEditable()) {
		test.that('We can edit a database.', 'test.db.editing', conn = conn)
		if (conn$isWritable()) {
			test.that("We cannot create another connector with the same URL.", 'test.create.conn.with.same.url', conn = conn)
			test.that('Database writing works.', 'test.db.writing', conn = conn)
			test.that('We can write entries having new fields.', 'test.db.writing.with.col.add', conn = conn)
			test.that('Database copy works.', 'test.db.copy', conn = conn)
		}
	}
}
