# vi: fdm=marker

# Test msmsSearch in Massbank {{{1
################################################################

test.msmsSearch.massbank <- function(db) {

	# Define spectrum to match:
	spectrum <- data.frame(mz = c(100.100, 83.100), rel.int = c(100, 10))

	# Search for match:
	result <- db$msmsSearch(spectrum, precursor.mz = 100, mz.tol = 0.3, ms.mode = 'neg', max.results = 3)

	expect_true( ! is.null(result))
	expect_true(is.data.frame(result))
	expect_true(nrow(result) > 0)
	cols <- c('id', 'score', paste('peak', seq(nrow(spectrum)), sep = '.'))
	expect_true(all(cols %in% colnames(result)))
}

# Test issue 150 InchiKEY computing loop in Massbank {{{1
################################################################

test.issue150.inchikey.computing.loop.in.massbank <- function(db) {

	entry <- db$getBiodb()$getFactory()$getEntry(db$getId(), 'KO002985', drop = TRUE)
	expect_true(is.na(entry$getFieldValue("inchikey", compute = FALSE)))
	expect_true(is.na(entry$getFieldValue("inchikey", compute = TRUE)))
}

# Test massbank relative intensity {{{1
################################################################

test.massbank.relative.intensity = function(conn, obs) {

	# Delete volatile cache entries
	conn$deleteAllCacheEntries()

	# Get reference entries
	ref.ids = list.ref.entries(conn$getDbClass())

	# Loop on all ref entry IDs
	for (id in ref.ids) {
		obs$clearMessages()
		entry = conn$getEntry(id)
		grep_msg = grep('^No peak has a relative intensity of 999 inside Massbank entry .*$', obs$getMsgsByType('caution'))
		testthat::expect_true(length(grep_msg) == 0, info = paste0('No peak with relative intensity of 999 was found in Massbank entry ', id, '.'))
	}
}

# Run Massbank tests {{{1
################################################################

run.massbank.tests <- function(conn, obs) {
	test.that('Relative intensity is parsed correctly.', 'test.massbank.relative.intensity', conn = conn, obs = obs)
	if (test.online()) {
		test.that('MSMS search works for Massbank.', 'test.msmsSearch.massbank', conn = conn)
		test.that('The computing of inchikey field in a Massbank entry does not loop indefinitely.', 'test.issue150.inchikey.computing.loop.in.massbank', conn = conn)
	}
}
