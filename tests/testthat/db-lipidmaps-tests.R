# vi: fdm=marker

# Test web service LMSDRecord {{{1
################################################################

test.lipidmaps.structure.ws.LMSDRecord = function(db) {

	results = db$ws.LMSDRecord(lmid = 'LMFA08040013')
	expect_is(results, 'character')
	expect_length(results, 1)

	# No parsing possible (output.type not selected)
	expect_error(db$ws.LMSDRecord(lmid = 'LMFA08040013', mode = 'File', retfmt = 'parsed'), regexp = '^.*Only TSV and CSV output types are parsable\\.$')

	# Parse results successfully
	results = db$ws.LMSDRecord(lmid = 'LMFA08040013', mode = 'File', output.type = 'CSV', retfmt = 'parsed')
	expect_is(results, 'data.frame')
	expect_equal(nrow(results), 1)
}

# Test web service LMSDSearch {{{1
################################################################

test.lipidmaps.structure.ws.LMSDSearch = function(db) {

	results = db$ws.LMSDSearch(mode = 'ProcessStrSearch', output.mode = 'File', lmid = 'LMSL02000001')
	expect_is(results, 'character')
	expect_length(results, 1)

	results = db$ws.LMSDSearch(mode = 'ProcessStrSearch', output.mode = 'File', lmid = 'LMSL02000001', retfmt = 'parsed')
	expect_is(results, 'data.frame')
	expect_equal(nrow(results), 1)

	results = db$ws.LMSDSearch(mode = 'ProcessStrSearch', output.mode = 'File', lmid = 'LMSL02000001', retfmt = 'ids')
	expect_is(results, 'character')
	expect_length(results, 1)
	expect_equal(results, 'LMSL02000001')

	results = db$ws.LMSDSearch(mode = 'ProcessStrSearch', output.mode = 'File', name = 'acid', retfmt = 'parsed')
	expect_is(results, 'data.frame')
	expect_gt(nrow(results), 0)

	results = db$ws.LMSDSearch(mode = 'ProcessStrSearch', output.mode = 'File', name = 'acid', exact.mass = 60.8, exact.mass.offset = 6, retfmt = 'parsed')
	expect_is(results, 'data.frame')
	expect_gt(nrow(results), 0)
}

# Run LipidMaps Structure tests {{{1
################################################################

run.lipidmaps.structure.tests = function(db, mode) {
	if (mode %in% c(MODE.ONLINE, MODE.QUICK.ONLINE)) {
		test.that("Test web service ws.LMSDRecord.", 'test.lipidmaps.structure.ws.LMSDRecord', conn = db)
		test.that("Test web service ws.LMSDSearch.", 'test.lipidmaps.structure.ws.LMSDSearch', conn = db)
	}
}
