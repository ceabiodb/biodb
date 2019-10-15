# vi: fdm=marker

# Test web service LMSDRecord {{{1
################################################################

test.lipidmaps.structure.wsLmsdRecord = function(db) {

	results = db$wsLmsdRecord(lmid = 'LMFA08040013')
	expect_is(results, 'character')
	expect_length(results, 1)

	# No parsing possible (output.type not selected)
	expect_error(db$wsLmsdRecord(lmid = 'LMFA08040013', mode = 'File', retfmt = 'parsed'), regexp = '^.*Only TSV and CSV output types are parsable\\.$')

	# Parse results successfully
	results = db$wsLmsdRecord(lmid = 'LMFA08040013', mode = 'File', output.type = 'CSV', retfmt = 'parsed')
	expect_is(results, 'data.frame')
	expect_equal(nrow(results), 1)
}

# Test web service LMSDSearch {{{1
################################################################

test.lipidmaps.structure.wsLmsdSearch = function(db) {

	results = db$wsLmsdSearch(mode = 'ProcessStrSearch', output.mode = 'File', lmid = 'LMSL02000001')
	expect_is(results, 'character')
	expect_length(results, 1)

	results = db$wsLmsdSearch(mode = 'ProcessStrSearch', output.mode = 'File', lmid = 'LMSL02000001', retfmt = 'parsed')
	expect_is(results, 'data.frame')
	expect_equal(nrow(results), 1)

	results = db$wsLmsdSearch(mode = 'ProcessStrSearch', output.mode = 'File', lmid = 'LMSL02000001', retfmt = 'ids')
	expect_is(results, 'character')
	expect_length(results, 1)
	expect_equal(results, 'LMSL02000001')

	results = db$wsLmsdSearch(mode = 'ProcessStrSearch', output.mode = 'File', name = 'acid', retfmt = 'parsed')
	expect_is(results, 'data.frame')
	expect_gt(nrow(results), 0)

	results = db$wsLmsdSearch(mode = 'ProcessStrSearch', output.mode = 'File', name = 'acid', exact.mass = 60.8, exact.mass.offset = 6, retfmt = 'parsed')
	expect_is(results, 'data.frame')
	expect_gt(nrow(results), 0)
}

# Main {{{1
################################################################

biodb::testThat("Test web service wsLmsdRecord.", test.lipidmaps.structure.wsLmsdRecord, conn = conn)
biodb::testThat("Test web service wsLmsdSearch.", test.lipidmaps.structure.wsLmsdSearch, conn = conn)
