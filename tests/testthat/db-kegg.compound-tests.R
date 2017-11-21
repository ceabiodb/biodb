# vi: fdm=marker

# Test KEGG ws.find {{{1
################################################################

test.kegg.ws.find <- function(db) {
	db$ws.find(query = '')
}

# Run KEGG Compound tests {{{1
################################################################

run.kegg.compound.tests <- function(db, mode) {
	if (mode %in% c(MODE.ONLINE, MODE.QUICK.ONLINE)) {
		run.db.test('Test', 'test.kegg.ws.find', db)
	}
}
