#########################
# DEFINE TEST FUNCTIONS #
#########################

# XXX Does not work. RUnit does not see the functions defined through assign() calls.
# for (online in c(TRUE, FALSE))
# 	for (db in c(BIODB.UNIPROT))
# 		for (entry.type in c(BIODB.SPECTRUM))
# 			if (BiodbFactory$new(useragent = USER.AGENT)$getConn(db)$handlesEntryType(entry.type)) {
# 				test.fct <- parse(text = paste0("function() { test.entries(", db, ", ", entry.type, ", online = ", online, ") }"))
# 				assign(paste(if (online) 'online' else 'offline', 'test', db, sep = '.'), test.fct)
# 			}

##############
# MASSFILEDB #
##############

offline.test.massfiledb <- function() {

	# Open file
	file <- file.path(dirname(script.path), 'res', 'massfiledb.tsv')
	df <- read.table(file, sep = "\t", header = TRUE)

	# Create factory
	factory <- BiodbFactory$new()

	# Create database
	db <- factory$getConn(BIODB.MASSFILEDB, url = file)
	fields <- list()
	db$setField(BIODB.ACCESSION, c('molid', 'mode', 'col'))
	db$setField(BIODB.COMPOUND.ID, 'molid')
	db$setField(BIODB.MSMODE, 'mode')
	db$setField(BIODB.PEAK.MZ, 'mztheo')
	db$setField(BIODB.PEAK.COMP, 'comp')
	db$setField(BIODB.PEAK.ATTR, 'attr')
	db$setField(BIODB.CHROM.COL, 'col')
	db$setField(BIODB.CHROM.COL.RT, 'colrt')
	db$setField(BIODB.FORMULA, 'molcomp')
	db$setField(BIODB.MASS, 'molmass')
	db$setField(BIODB.FULLNAMES, 'molnames')
	db$setMsMode(BIODB.MSMODE.NEG, 'NEG')
	db$setMsMode(BIODB.MSMODE.POS, 'POS')

	# Run general tests
	test.db(db)

	# Generic tests

		# Test entries
		checkTrue(db$getNbEntries() > 1)

		# Test chrom cols
		entry.id <- db$getEntryIds()[[1]]
		checkTrue(nrow(db$getChromCol()) > 1)
		checkTrue(nrow(db$getChromCol(entry.id)) > 1)
		checkTrue(nrow(db$getChromCol(entry.id)) < nrow(db$getChromCol()))
		checkTrue(all(db$getChromCol(entry.id)[[BIODB.ID]] %in% db$getChromCol()[[BIODB.ID]]))

		# Test mz values
		checkTrue(is.vector(db$getMzValues()))
		checkTrue(length(db$getMzValues()) > 1)
		checkException(db$getMzValues('wrong.mode.value'), silent = TRUE)
		checkTrue(length(db$getMzValues(BIODB.MSMODE.NEG)) > 1)
		checkTrue(length(db$getMzValues(BIODB.MSMODE.POS)) > 1)

	# Specific tests for filedb
		# Test entry ids
		checkEquals(db$getEntryIds(), sort(as.character(df[! duplicated(df['molid']), 'molid'])))

		# Test nb entries
		checkTrue(db$getNbEntries() == sum(as.integer(! duplicated(df['molid']))))
}

###########
# UNIPROT #
###########

offline.test.uniprot <- function() {
	test.entries(BIODB.UNIPROT, online = FALSE)
}

online.test.uniprot <- function() {
	test.entries(BIODB.UNIPROT, online = TRUE)
}

############
# NCBICCDS #
############

offline.test.ncbiccds <- function() {
	test.entries(BIODB.NCBICCDS, online = FALSE)
}

online.test.ncbiccds <- function() {
	test.entries(BIODB.NCBICCDS, online = TRUE)
}

############
# NCBIGENE #
############

offline.test.ncbigene <- function() {
	test.entries(BIODB.NCBIGENE, online = FALSE)
}

online.test.ncbigene <- function() {
	test.entries(BIODB.NCBIGENE, online = TRUE)
}

###########
# MIRBASE #
###########

offline.test.mirbase <- function() {
	test.entries(BIODB.MIRBASE, online = FALSE)
}

online.test.mirbase <- function() {
	test.entries(BIODB.MIRBASE, online = TRUE)
}

#############
# LIPIDMAPS #
#############

offline.test.lipidmaps <- function() {
	test.entries(BIODB.LIPIDMAPS, online = FALSE)
}

online.test.lipidmaps <- function() {
	test.entries(BIODB.LIPIDMAPS, online = TRUE)
}

##########
# ENZYME #
##########

offline.test.enzyme <- function() {
	test.entries(BIODB.ENZYME, online = FALSE)
}

online.test.enzyme <- function() {
	test.entries(BIODB.ENZYME, online = TRUE)
}

##############
# CHEMSPIDER #
##############

offline.test.chemspider <- function() {
	test.entries(BIODB.CHEMSPIDER, online = FALSE)
}

online.test.chemspider <- function() {
	test.entries(BIODB.CHEMSPIDER, online = TRUE)
}

########
# HMDB #
########

offline.test.hmdb <- function() {
	test.entries(BIODB.HMDB, online = FALSE)
}

online.test.hmdb <- function() {
	test.entries(BIODB.HMDB, online = TRUE)
}

####################
# PUBCHEM COMPOUND #
####################

offline.test.pubchemcomp <- function() {
	test.entries(BIODB.PUBCHEMCOMP, online = FALSE)
}

online.test.pubchemcomp <- function() {
	test.entries(BIODB.PUBCHEMCOMP, online = TRUE)
}

#########
# CHEBI #
#########

offline.test.chebi <- function() {
	test.entries(BIODB.CHEBI, online = FALSE)
}

online.test.chebi <- function() {
	test.entries(BIODB.CHEBI, online = TRUE)
}

########
# KEGG #
########

offline.test.kegg <- function() {
	test.entries(BIODB.KEGG, online = FALSE)
}

online.test.kegg <- function() {
	test.entries(BIODB.KEGG, online = TRUE)
}

############
# MASSBANK #
############

offline.test.massbank <- function() {
	test.entries(BIODB.MASSBANK, online = FALSE)
}

online.test.massbank <- function() {
	test.entries(BIODB.MASSBANK, online = TRUE)
}
