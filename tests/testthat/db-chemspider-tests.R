# vi: fdm=marker

# Test ChemSpider ws.SearchByMass2
################################################################

test.chemspider.ws.SearchByMass2 <- function(db) {
	db$ws.SearchByMass2(mass = , range = )
}

# Run ChemSpider tests {{{1
################################################################

run.chemspider.tests <- function(db, mode) {
	if (mode %in% c(MODE.ONLINE, MODE.QUICK.ONLINE))
		run.db.test('ChemSpider web service search by mass works fine.', 'test.chemspider.ws.SearchByMass2', db)
}
