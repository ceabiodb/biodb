# vi: fdm=marker

# Test ChemSpider ws.SearchByMass2
################################################################

test.chemspider.ws.SearchByMass2 <- function(db) {

	results <- db$ws.SearchByMass2(mass = 96, range = 0.01)
	print(results)
	expect_true(nchar(results) > 0)

	ids <- db$ws.SearchByMass2.ids(mass = 96, range = 0.01)
	print(ids)
	expect_true(is.character(ids))
	expect_true(length(ids) > 0)
}

# Run ChemSpider tests {{{1
################################################################

run.chemspider.tests <- function(db, mode) {
	if (mode %in% c(MODE.ONLINE, MODE.QUICK.ONLINE))
		run.db.test('ChemSpider web service search by mass works fine.', 'test.chemspider.ws.SearchByMass2', db)
}
