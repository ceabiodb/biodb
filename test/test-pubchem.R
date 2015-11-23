#############
# CONSTANTS #
#############

PUBCHEM.ENTRIES <- list(
	list(type = RBIODB.PUBCHEM, id = '2', inchi = 'InChI=1S/C9H17NO4/c1-7(11)14-8(5-9(12)13)6-10(2,3)4/h8H,5-6H2,1-4H3/p+1', inchikey = 'RDHQFKQIGNGIED-UHFFFAOYSA-O'),
	list(type = RBIODB.PUBCHEM, id = 'ZOP', false = TRUE)
)

#######################
# ONLINE TEST PUBCHEM #
#######################

online.test.pubchem <- function() {
	online.test(PUBCHEM.ENTRIES)
}
