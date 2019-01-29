# vi: fdm=marker

# Test KEGG Compound ws.find {{{1
################################################################

test.kegg.compound.ws.find <- function(db) {

	results <- db$ws.find(query = 'NADPH')
	expect_true( ! is.null(results))
	expect_true( ! is.na(results))
	expect_true(is.character(results))
	readtc <- textConnection(results, "r", local = TRUE)
	df <- read.table(readtc, sep = "\t", quote = '', stringsAsFactors = FALSE)
	expect_true(nrow(df) > 1)
	expect_true(df[1, 1] == 'cpd:C00005')

	df.2 <- db$ws.find.df(query = 'NADPH') 
	expect_true(identical(df, df.2))

	ids <- db$ws.find.ids(query = 'NADPH') 
	expect_true(identical(ids, df[[1]]))
	expect_true(ids[[1]] == 'cpd:C00005')
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
	df.2 <- db$ws.find.exact.mass.df(mass = 174.05)
	expect_true(identical(df, df.2))

	# Test IDs
	ids <- db$ws.find.exact.mass.ids(mass = 174.05)
	expect_true( ! is.null(ids))
	expect_true( all(! is.na(ids)))
	expect_true(is.character(ids))
	expect_true(identical(df[[1]], ids))

	# Test mass range
	ids <- db$ws.find.exact.mass.ids(mass.min = 174, mass.max = 174.35)
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
	df.2 <- db$ws.find.molecular.weight.df(mass = 300)
	expect_true(identical(df, df.2))

	# Test IDs
	ids <- db$ws.find.molecular.weight.ids(mass = 300)
	expect_true( ! is.null(ids))
	expect_true( all(! is.na(ids)))
	expect_true(is.character(ids))
	expect_true(identical(df[[1]], ids))

	# Test mass range
	ids <- db$ws.find.molecular.weight.ids(mass.min = 300, mass.max = 310)
	expect_true( ! is.null(ids))
	expect_true( all(! is.na(ids)))
	expect_true(is.character(ids))
	expect_true(length(ids) > 1)
}

# Run KEGG Compound tests {{{1
################################################################

run.kegg.compound.tests <- function(db, mode) {
	if (mode %in% c(MODE.ONLINE, MODE.QUICK.ONLINE)) {
		test.that('Test', 'test.kegg.compound.ws.find', conn = db)
		test.that('Test', 'test.kegg.compound.ws.find.exact.mass', conn = db)
		test.that('Test', 'test.kegg.compound.ws.find.molecular.weight', conn = db)
	}
}
