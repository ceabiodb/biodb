#########
# CHEBI #
#########

offline.test.chebi <- function() {
	test.entries(RBIODB.CHEBI, RBIODB.COMPOUND, online = FALSE)
}

online.test.chebi <- function() {
	test.entries(RBIODB.CHEBI, RBIODB.COMPOUND, online = TRUE)
}

########
# KEGG #
########

offline.test.kegg <- function() {
	test.entries(RBIODB.KEGG, RBIODB.COMPOUND, online = FALSE)
}

online.test.kegg <- function() {
	test.entries(RBIODB.KEGG, RBIODB.COMPOUND, online = TRUE)
}

############
# MASSBANK #
############

offline.test.massbank <- function() {
	test.entries(RBIODB.MASSBANK, RBIODB.SPECTRUM, online = FALSE)
}

online.test.massbank <- function() {
	test.entries(RBIODB.MASSBANK, RBIODB.SPECTRUM, online = TRUE)
}
