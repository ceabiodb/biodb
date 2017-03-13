# vi: fdm=marker

source('common.R')

# Test msmsSearch {{{1
################################################################

test.msmsSearch <- function(biodb, db) {
	m <- matrix(c(
		310.05995, 0.26,
		254.098, 0.32,
		252.1009, 1.2,
		263.13292, 0.35,
		320.08807, 0.78,
		208.1113, 0.05,
		331.11925, 0.02,
		321.09038, 0.03,
		330.11694, 1.3,
		322.08591, 0.19,
		172.13425, 0.2,
		216.12395, 0.19,
		262.12984, 3.35,
		312.05596, 0.2,
		253.10365, 0.08), ncol = 2, byrow = TRUE)
	spectrum <- data.frame(m)
	colnames(spectrum) <- c('mz', 'intensity')
	result <- biodb$getFactory()$getConn(db)$msmsSearch(spectrum, precursor = 240, mztol = 0.1, tolunit = BIODB.MZTOLUNIT.PLAIN, mode = BIODB.MSMODE.NEG, npmin = 2, fun = 'pbachtttarya', params = list(ppm = 3, dmz = 0.005, mzexp = 2, intexp = 0.5))
	expect_true( ! is.null(result))
	expect_true(length(result$matchedpeaks) > 0)
}

# MAIN {{{1
################################################################

for (db in c(BIODB.PEAKFOREST.LCMS)) {

	if (db %in% TEST.DATABASES) {

		if (MODE.ONLINE %in% TEST.MODES) {

			# Create biodb instance
			biodb <- create.biodb.instance()
			set.test.context(biodb, "Testing MSMS features")
			set.mode(biodb, MODE.ONLINE)

			test_that("MSMS search returns at least one match", test.msmsSearch(biodb, db))
		}
	}
}
