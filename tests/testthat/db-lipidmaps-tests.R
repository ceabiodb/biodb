# vi: fdm=marker

# Test web service LMSDRecord {{{1
################################################################

test.lipidmaps.structure.ws.LMSDRecord <- function(db) {
	results <- db$ws.LMSDRecord(mode = 'File', lmid = 'LMFA08040013')
	expect_is(results, 'character')
	expect_length(results, 1)
}

# Run LipidMaps Structure tests {{{1
################################################################

run.lipidmaps.structure.tests <- function(db, mode) {
	if (mode %in% c(MODE.ONLINE, MODE.QUICK.ONLINE))
		run.db.test.that("Test web service ws.LMSDRecord.", 'test.lipidmaps.structure.ws.LMSDRecord', db)
}
