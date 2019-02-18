#############
# DATABASES #
#############

BIODB.DATABASES <- sort(c('chebi', 'chemspider', 'expasy.enzyme', 'hmdb.metabolites', 'kegg.compound', 'lipidmaps.structure', 'massbank', 'mass.csv.file', 'mirbase.mature', 'ncbi.ccds', 'ncbi.gene', 'peakforest.mass', 'peakforest.compound', 'ncbi.pubchem.comp', 'ncbi.pubchem.subst', 'uniprot'))
for (db in BIODB.DATABASES) {

	# Create constant for database name
	assign(toupper(paste('biodb', db, sep = '.')), db)
}

# DEPRECATED Mode values
BIODB.MSMODE.NEG <- 'neg'
BIODB.MSMODE.POS <- 'pos'

# Tolerance values
BIODB.TOL <- 'mztol'
BIODB.MZTOLUNIT.PPM <- 'ppm'
BIODB.MZTOLUNIT.PLAIN <- 'plain' # same as mz: mass-to-charge ratio
BIODB.MZTOLUNIT.VALS <- c(BIODB.MZTOLUNIT.PPM, BIODB.MZTOLUNIT.PLAIN)

########################
# MS-MS MEASURE VALUES #
########################

BIODB.MSMS.DIST.COS <- "cosine"
BIODB.MSMS.DIST.WCOSINE <- "wcosine"
BIODB.MSMS.DIST.PKERNEL <- "pkernel"
BIODB.MSMS.DIST.PBACH <- "bachtttarya"
