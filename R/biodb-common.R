#############
# DATABASES #
#############

BIODB.DATABASES <- sort(c('chebi', 'chemspider', 'expasy.enzyme', 'hmdb.metabolite', 'kegg.compound', 'lipidmaps.structure', 'massbank.eu', 'massbank.jp', 'mass.csv.file', 'mirbase.mature', 'ncbi.ccds', 'ncbi.gene', 'peakforest.mass', 'peakforest.compound', 'ncbi.pubchem.comp', 'ncbi.pubchem.subst', 'uniprot'))
for (db in BIODB.DATABASES) {

	# Create constant for database name
	assign(toupper(paste('biodb', db, sep = '.')), db)
}

# Mode values
BIODB.MSMODE.NEG <- 'neg'
BIODB.MSMODE.POS <- 'pos'
BIODB.MSMODE.VALS <- c(BIODB.MSMODE.NEG, BIODB.MSMODE.POS)

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

#####################
# COMPUTABLE FIELDS #
#####################

BIODB.FIELD.COMPUTING <- list()
BIODB.FIELD.COMPUTING[['inchi']]      <- c('chebi')
BIODB.FIELD.COMPUTING[['inchikey']]   <- c('chebi')
BIODB.FIELD.COMPUTING[['sequence']]   <- c('ncbi.ccds')

####################
# PEAKS DATA FRAME #
####################

# Peak fields

BIODB.PEAK.FIELDS <- c('peak.mz', 'peak.intensity', 'peak.relative.intensity', 'peak.formula', 'peak.formula.count', 'peak.mass', 'peak.error.ppm', 'peak.mzexp', 'peak.mztheo', 'peak.comp', 'peak.attr')

# Example
BIODB.PEAK.DF.EXAMPLE <- data.frame(mz = double(), int = double(), rel.int = integer(), formula = character(), formula.count <- integer(), mass = double(), error = double(), stringsAsFactors = FALSE)
colnames(BIODB.PEAK.DF.EXAMPLE) <- c('peak.mz', 'peak.intensity', 'peak.relative.intensity', 'peak.formula', 'peak.formula.count', 'peak.mass', 'peak.error.ppm')
