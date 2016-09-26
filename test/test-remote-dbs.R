####################
# TEST WRONG ENTRY #
####################

test_wrong_entry <- function(factory, db) {

	# Test a wrong accession number
	wrong.entry <- factory$createEntry(db, id = 'WRONG')
	expect_null(wrong.entry)
}

#####################
# TEST ENTRY FIELDS #
#####################

test_entry_fields <- function(factory, db) {

	# Download contents
	entries.file <- file.path(dirname(script.path), 'res', paste0(db, '-entries.txt'))
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

########
# MAIN #
########

# Set online/offline modes to test
online.modes = logical()
if ( ! is.null(opt[['offline']]))
	online.modes <- c(online.modes, FALSE)
if ( ! is.null(opt[['online']]))
	online.modes <- c(online.modes, TRUE)

# Loop on online/offline modes
for (online in online.modes) {

	# Create factory
	factory <- BiodbFactory$new(useragent = USER.AGENT,
								cache.dir = file.path(dirname(script.path), if (online) 'cache' else file.path('res', 'offline-files')),
								cache.mode = if (online) BIODB.CACHE.WRITE.ONLY else BIODB.CACHE.READ.ONLY,
								debug = ! is.null(opt[['debug']]),
								use.env.var = TRUE
								)

	# Loop on all databases
	for (db in BIODB.ONLINE.DATABASES) {
		if (is.null(opt[['databases']]) || db %in% opt[['databases']]) {
			context(paste0("Testing database ", db, if (online) " online" else " offline"))
			test_that("Wrong entry gives NULL", test_wrong_entry(factory, db))
			test_that("Entry fields have a correct value", test_entry_fields(factory, db))
		}
	}
}
