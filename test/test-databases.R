#########################
# DEFINE TEST FUNCTIONS #
#########################

# XXX Does not work. RUnit does not see the functions defined through assign() calls.
# for (online in c(TRUE, FALSE))
# 	for (db in c(BIODB.UNIPROT))
# 		for (entry.type in c(BIODB.COMPOUND, BIODB.SPECTRUM))
# 			if (BiodbFactory$new(useragent = USER.AGENT)$getConn(db)$handlesEntryType(entry.type)) {
# 				test.fct <- parse(text = paste0("function() { test.entries(", db, ", ", entry.type, ", online = ", online, ") }"))
# 				assign(paste(if (online) 'online' else 'offline', 'test', db, sep = '.'), test.fct)
# 			}

##########
# FILEDB #
##########

offline.test.filedb <- function() {
	test.db(BIODB.FILEDB, online = FALSE)
}

###########
# UNIPROT #
###########

offline.test.uniprot <- function() {
	test.entries(BIODB.UNIPROT, BIODB.COMPOUND, online = FALSE)
}

online.test.uniprot <- function() {
	test.entries(BIODB.UNIPROT, BIODB.COMPOUND, online = TRUE)
}

############
# NCBICCDS #
############

offline.test.ncbiccds <- function() {
	test.entries(BIODB.NCBICCDS, BIODB.COMPOUND, online = FALSE)
}

online.test.ncbiccds <- function() {
	test.entries(BIODB.NCBICCDS, BIODB.COMPOUND, online = TRUE)
}

############
# NCBIGENE #
############

offline.test.ncbigene <- function() {
	test.entries(BIODB.NCBIGENE, BIODB.COMPOUND, online = FALSE)
}

online.test.ncbigene <- function() {
	test.entries(BIODB.NCBIGENE, BIODB.COMPOUND, online = TRUE)
}

###########
# MIRBASE #
###########

offline.test.mirbase <- function() {
	test.entries(BIODB.MIRBASE, BIODB.COMPOUND, online = FALSE)
}

online.test.mirbase <- function() {
	test.entries(BIODB.MIRBASE, BIODB.COMPOUND, online = TRUE)
}

#############
# LIPIDMAPS #
#############

offline.test.lipidmaps <- function() {
	test.entries(BIODB.LIPIDMAPS, BIODB.COMPOUND, online = FALSE)
}

online.test.lipidmaps <- function() {
	test.entries(BIODB.LIPIDMAPS, BIODB.COMPOUND, online = TRUE)
}

##########
# ENZYME #
##########

offline.test.enzyme <- function() {
	test.entries(BIODB.ENZYME, BIODB.COMPOUND, online = FALSE)
}

online.test.enzyme <- function() {
	test.entries(BIODB.ENZYME, BIODB.COMPOUND, online = TRUE)
}

##############
# CHEMSPIDER #
##############

offline.test.chemspider <- function() {
	test.entries(BIODB.CHEMSPIDER, BIODB.COMPOUND, online = FALSE)
}

online.test.chemspider <- function() {
	test.entries(BIODB.CHEMSPIDER, BIODB.COMPOUND, online = TRUE)
}

########
# HMDB #
########

offline.test.hmdb <- function() {
	test.entries(BIODB.HMDB, BIODB.COMPOUND, online = FALSE)
}

online.test.hmdb <- function() {
	test.entries(BIODB.HMDB, BIODB.COMPOUND, online = TRUE)
}

###########
# PUBCHEM #
###########

offline.test.pubchem <- function() {
	test.entries(BIODB.PUBCHEM, BIODB.COMPOUND, online = FALSE)
}

online.test.pubchem <- function() {
	test.entries(BIODB.PUBCHEM, BIODB.COMPOUND, online = TRUE)
}

#########
# CHEBI #
#########

offline.test.chebi <- function() {
	test.entries(BIODB.CHEBI, BIODB.COMPOUND, online = FALSE)
}

online.test.chebi <- function() {
	test.entries(BIODB.CHEBI, BIODB.COMPOUND, online = TRUE)
}

########
# KEGG #
########

offline.test.kegg <- function() {
	test.entries(BIODB.KEGG, BIODB.COMPOUND, online = FALSE)
}

online.test.kegg <- function() {
	test.entries(BIODB.KEGG, BIODB.COMPOUND, online = TRUE)
}

############
# MASSBANK #
############

offline.test.massbank <- function() {
	test.entries(BIODB.MASSBANK, BIODB.SPECTRUM, online = FALSE)
}

online.test.massbank <- function() {
	test.entries(BIODB.MASSBANK, BIODB.SPECTRUM, online = TRUE)
}
