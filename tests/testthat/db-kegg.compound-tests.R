# vi: fdm=marker

# Test KEGG Compound ws.list {{{1
################################################################

test.kegg.compound.ws.list <- function(conn) {

	results <- conn$ws.list(retfmt = 'ids')
	testthat::expect_is(results, 'character')
	testthat::expect_true(length(results) > 100)
}

# Test KEGG Compound ws.find {{{1
################################################################

test.kegg.compound.ws.find <- function(db) {

	results <- db$ws.find(query = 'NADPH')
	testthat::expect_true( ! is.null(results))
	testthat::expect_true( ! is.na(results))
	testthat::expect_true(is.character(results))
	readtc <- textConnection(results, "r", local = TRUE)
	df <- read.table(readtc, sep = "\t", quote = '', stringsAsFactors = FALSE)
	testthat::expect_true(nrow(df) > 1)
	testthat::expect_true(df[1, 1] == 'cpd:C00005')

	df.2 <- db$ws.find(query = 'NADPH', retfmt = 'parsed') 
	testthat::expect_is(df.2, 'data.frame')
	testthat::expect_true(identical(df, df.2))

	ids <- db$ws.find(query = 'NADPH', retfmt = 'ids') 
	testthat::expect_is(ids, 'character')
	testthat::expect_true(identical(ids, df[[1]]))
	testthat::expect_true(ids[[1]] == 'cpd:C00005')
}

# Test KEGG Compound ws.find.exact.mass {{{1
################################################################

test.kegg.compound.ws.find.exact.mass <- function(db) {

	# Test single mass
	results <- db$ws.find.exact.mass(mass = 174.05)
	expect_true( ! is.null(results))
	expect_true( ! is.na(results))
	expect_true(is.character(results))
	readtc <- textConnection(results, "r", local = TRUE)
	df <- read.table(readtc, sep = "\t", quote = '', stringsAsFactors = FALSE)
	expect_true(nrow(df) > 1)

	# Test data frame
	df.2 <- db$ws.find.exact.mass(mass = 174.05, retfmt = 'parsed')
	expect_true(identical(df, df.2))

	# Test IDs
	ids <- db$ws.find.exact.mass(mass = 174.05, retfmt = 'ids')
	expect_true( ! is.null(ids))
	expect_true( all(! is.na(ids)))
	expect_true(is.character(ids))
	expect_true(identical(df[[1]], ids))

	# Test mass range
	ids <- db$ws.find.exact.mass(mass.min = 174, mass.max = 174.35, retfmt = 'ids')
	expect_true( ! is.null(ids))
	expect_true( all(! is.na(ids)))
	expect_true(is.character(ids))
	expect_true(length(ids) > 1)
}

# Test KEGG Compound ws.find.molecular.weight {{{1
################################################################

test.kegg.compound.ws.find.molecular.weight <- function(db) {

	# Test single mass
	results <- db$ws.find.molecular.weight(mass = 300)
	expect_true( ! is.null(results))
	expect_true( ! is.na(results))
	expect_true(is.character(results))
	readtc <- textConnection(results, "r", local = TRUE)
	df <- read.table(readtc, sep = "\t", quote = '', stringsAsFactors = FALSE)
	expect_true(nrow(df) > 1)

	# Test data frame
	df.2 <- db$ws.find.molecular.weight(mass = 300, retfmt = 'parsed')
	expect_true(identical(df, df.2))

	# Test IDs
	ids <- db$ws.find.molecular.weight(mass = 300, retfmt = 'ids')
	expect_true( ! is.null(ids))
	expect_true( all(! is.na(ids)))
	expect_true(is.character(ids))
	expect_true(identical(df[[1]], ids))

	# Test mass range
	ids <- db$ws.find.molecular.weight(mass.min = 300, mass.max = 310, retfmt = 'ids')
	expect_true( ! is.null(ids))
	expect_true( all(! is.na(ids)))
	expect_true(is.character(ids))
	expect_true(length(ids) > 1)
}

# Test KEGG Compound getPathwayIds() {{{1
################################################################

test.kegg.compound.getPathwayIds <- function(conn) {
	c = 'C00134'
	ids = conn$getPathwayIds(c, 'mmu')
	testthat::expect_is(ids, 'character')
	testthat::expect_true(length(ids) > 0)
}

# Test KEGG Compound getPathwayIdsPerCompound() {{{1
################################################################

test.kegg.compound.getPathwayIdsPerCompound <- function(conn) {
	c = 'C00134'
	ids = conn$getPathwayIdsPerCompound(c, 'mmu')
	testthat::expect_is(ids, 'list')
	testthat::expect_true(c %in% names(ids))
	testthat::expect_is(ids[[c]], 'character')
	testthat::expect_true(length(ids[[c]]) > 0)
}

# Run KEGG Compound tests {{{1
################################################################

run.kegg.compound.tests <- function(conn, obs) {
	if (test.online()) {
		test.that('ws.list() works correctly.', 'test.kegg.compound.ws.list', conn = conn)
		test.that('ws.find() works correctly.', 'test.kegg.compound.ws.find', conn = conn)
		test.that('ws.find.exact.mass() works correctly.', 'test.kegg.compound.ws.find.exact.mass', conn = conn)
		test.that('ws.find.molecular.weight() works correctly.', 'test.kegg.compound.ws.find.molecular.weight', conn = conn)
		test.that('getPathwayIdsPerCompound() works correctly.', 'test.kegg.compound.getPathwayIdsPerCompound', conn = conn)
		test.that('getPathwayIds() works correctly.', 'test.kegg.compound.getPathwayIds', conn = conn)
	}
}
