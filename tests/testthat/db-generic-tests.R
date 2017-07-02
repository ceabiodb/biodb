# vi: fdm=marker

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

# Test wrong entry {{{1
################################################################

test.wrong.entry <- function(biodb, db) {

	# Test a wrong accession number
	wrong.entry <- biodb$getFactory()$getEntry(db, id = 'WRONGA')
	expect_null(wrong.entry)
}

# Test wrong entry among good ones {{{1
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
	expect_true(is.character(ids))
	n <- length(ids)
	expect_true(n >= 0 && n <= max)
}

