# vi: fdm=marker

# Test msmsSearch massbank {{{1
################################################################

test.msmsSearch.massbank <- function(db) {

	# Define spectrum to match:
	spectrum <- data.frame(mz = c(100.100, 83.100), rel.int = c(100, 10))

	# Search for match:
	result <- db$msmsSearch(spectrum, precursor.mz = 100, mz.tol = 0.3)

	expect_true( ! is.null(result))
	expect_true(is.data.frame(result))
	expect_true(nrow(result) > 0)
	cols <- c('id', 'score', paste('peak', seq(nrow(spectrum)), sep = '.'))
	expect_true(all(cols %in% colnames(result)))
}

# Run Massbank Japan tests {{{1
################################################################

run.massbank.jp.tests <- function(db, mode) {
	if (mode %in% c(MODE.ONLINE, MODE.QUICK.ONLINE))
		run.db.test.that('MSMS search works for massbank.', 'test.msmsSearch.massbank', db)
}
