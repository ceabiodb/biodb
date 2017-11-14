# vi: fdm=marker

# Save entries as JSON {{{1
################################################################

save.entries.as.json <- function(db, entries) {

	# Loop on all entries
	for (e in entries) {
		json <- e$getFieldsAsJson()
		writeChar(json, file.path(OUTPUT.DIR, paste(db, '-entry-', e$getFieldValue('accession'), '.json', sep = '')))
	}
}

# Test entry fields {{{1
################################################################

test.entry.fields <- function(db) {

	biodb <- db$getBiodb()
	db.name <- db$getId()

	# Load reference entries
	entries.desc <- load.ref.entries(db.name)

	# Create entries
	entries <- biodb$getFactory()$getEntry(db.name, id = entries.desc[['accession']], drop = FALSE)
	expect_false(any(vapply(entries, is.null, FUN.VALUE = TRUE)), "One of the entries is NULL.")
	expect_equal(length(entries), nrow(entries.desc), info = paste0("Error while retrieving entries. ", length(entries), " entrie(s) obtained instead of ", nrow(entries.desc), "."))
	save.entries.as.json(db.name, entries)

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
		biodb$message('caution', paste("Fields \"", paste(not.tested.fields, collapse = ", "), "\" are not tested in database ", db.name, ".", sep = ''))
	}
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
	biodb$message('debug', 'test.wrong.entry.among.good.ones 02')
	entries <- biodb$getFactory()$getEntry(db.name, id = c('WRONGB', entries.desc[['accession']]))
	expect_equal(length(entries), nrow(entries.desc) + 1, info = paste0("Error while retrieving entries. ", length(entries), " entrie(s) obtained instead of ", nrow(entries.desc) + 1, "."))
	expect_null(entries[[1]])
	biodb$message('debug', 'test.wrong.entry.among.good.ones 10')
	expect_false(any(vapply(entries[2:length(entries)], is.null, FUN.VALUE = TRUE)))
	biodb$message('debug', 'test.wrong.entry.among.good.ones 20')
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

# Run db generic tests {{{1
################################################################

run.db.generic.tests <- function(db) {
	run.db.test("Nb entries is positive", 'test.nb.entries', db)
	run.db.test("We can get a list of entry ids", 'test.entry.ids', db)
}
