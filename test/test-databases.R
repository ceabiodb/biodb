##############
# CHEMSPIDER #
##############

offline.test.chemspider <- function() {
	test.entries(RBIODB.CHEMSPIDER, RBIODB.COMPOUND, online = FALSE)
}

online.test.chemspider <- function() {
	test.entries(RBIODB.CHEMSPIDER, RBIODB.COMPOUND, online = TRUE)
}

########
# HMDB #
########

offline.test.hmdb <- function() {
	test.entries(RBIODB.HMDB, RBIODB.COMPOUND, online = FALSE)
}

online.test.hmdb <- function() {
	test.entries(RBIODB.HMDB, RBIODB.COMPOUND, online = TRUE)
}

###########
# PUBCHEM #
###########

offline.test.pubchem <- function() {
	test.entries(RBIODB.PUBCHEM, RBIODB.COMPOUND, online = FALSE)
}

online.test.pubchem <- function() {
	test.entries(RBIODB.PUBCHEM, RBIODB.COMPOUND, online = TRUE)
}

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
