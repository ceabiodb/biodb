# vi: fdm=marker

# Test KEGG Compound ws.find {{{1
################################################################

test.kegg.compound.ws.find <- function(db) {

	results <- db$ws.find(query = 'NADPH')
	expect_true( ! is.null(results))
	expect_true( ! is.na(results))
	expect_true(is.character(results))
	readtc <- textConnection(results, "r", local = TRUE)
	df <- read.table(readtc, sep = "\t", quote = '')
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

	results <- db$ws.find.exact.mass(mass = 174.05)
	expect_true( ! is.null(results))
	expect_true( ! is.na(results))
	expect_true(is.character(results))
	readtc <- textConnection(results, "r", local = TRUE)
	df <- read.table(readtc, sep = "\t", quote = '')
	expect_true(nrow(df) > 1)
}

# Run KEGG Compound tests {{{1
################################################################

run.kegg.compound.tests <- function(db, mode) {
	if (mode %in% c(MODE.ONLINE, MODE.QUICK.ONLINE)) {
		run.db.test('Test', 'test.kegg.compound.ws.find', db)
		run.db.test('Test', 'test.kegg.compound.ws.find.exact.mass', db)
	}
}
