#######################
# ENTRY CONTENT TYPES #
#######################

BIODB.HTML <- 'html'
BIODB.TXT  <- 'txt'
BIODB.XML  <- 'xml'
BIODB.CSV  <- 'csv'
BIODB.TSV  <- 'tsv'
BIODB.JSON <- 'json'

BIODB.CONTENT.TYPES <- c(BIODB.HTML, BIODB.TXT, BIODB.XML, BIODB.CSV, BIODB.TSV, BIODB.JSON)

#############
# DATABASES #
#############

BIODB.DATABASES <- sort(c('chebi', 'chemspider', 'expasy.enzyme', 'hmdb.metabolite', 'kegg.compound', 'lipidmaps.structure', 'massbank', 'mass.csv.file', 'mirbase.mature', 'ncbi.ccds', 'ncbi.gene', 'peakforest.lcms', 'peakforest.compound', 'ncbi.pubchem.comp', 'ncbi.pubchem.subst', 'uniprot'))
for (db in BIODB.DATABASES) {

	# Create constant for database name
	assign(toupper(paste('biodb', db, sep = '.')), db)

	# Create constant for database ID
	assign(toupper(paste('biodb', db, 'id', sep = '.')), paste(db, 'id', sep = '.'))
}

##########
# FIELDS #
##########

BIODB.ACCESSION    <- 'accession'
BIODB.DESCRIPTION  <- 'description'
BIODB.PROTEIN.DESCRIPTION  <- 'protdesc'
BIODB.NAME         <- 'name'
BIODB.COMP.IUPAC.NAME.ALLOWED  <- 'comp.iupac.name.allowed'
BIODB.COMP.IUPAC.NAME.TRAD     <- 'comp.iupac.name.trad'
BIODB.COMP.IUPAC.NAME.SYST     <- 'comp.iupac.name.syst'
BIODB.COMP.IUPAC.NAME.PREF     <- 'comp.iupac.name.pref'
BIODB.COMP.IUPAC.NAME.CAS      <- 'comp.iupac.name.cas'
BIODB.COMP.LOGP <- 'logp'
BIODB.FULLNAMES                 <- 'fullnames'
BIODB.SYNONYMS                  <- 'synonyms'
BIODB.SYMBOL                    <- 'symbol'
BIODB.GENE.SYMBOLS              <- 'genesymbols'
BIODB.CAS.ID                    <- 'casid'
BIODB.SMILES        <- 'smiles'
BIODB.SMILES.CANONICAL        <- 'smiles.canonical'
BIODB.SMILES.ISOMERIC        <- 'smiles.isomeric'
BIODB.INCHI        <- 'inchi'
BIODB.INCHIKEY     <- 'inchikey'
BIODB.MSDEV        <- 'msdev'
BIODB.MSDEVTYPE    <- 'msdevtype'
BIODB.MSTYPE       <- 'mstype'
BIODB.MS.LEVEL     <- 'ms.level'
BIODB.MSMODE       <- 'msmode'
BIODB.MSPRECMZ     <- 'msprecmz'       # numeric
BIODB.MSPRECANNOT  <- 'msprecannot'
BIODB.FORMULA      <- 'formula'
BIODB.SUPER.CLASS  <- 'superclass'
BIODB.MASS         <- 'mass'
BIODB.AVERAGE.MASS <- 'average.mass'
BIODB.MONOISOTOPIC.MASS <- 'monoisotopic.mass'
BIODB.NOMINAL.MASS <- 'nominal.mass'
BIODB.MOLECULAR.WEIGHT <- 'molecular.weight'
BIODB.SEQUENCE     <- 'sequence'
BIODB.LOCATION     <- 'location'
BIODB.LENGTH       <- 'length'
BIODB.NB.PEAKS     <- 'nbpeaks'
BIODB.PEAKS        <- 'peaks'
BIODB.COMPOUNDS    <- 'compounds'
BIODB.NB.COMPOUNDS <- 'nbcompounds'
BIODB.COMPOUND.ID   <- 'compoundid'
BIODB.COMPOUND.MASS   <- 'compoundmass'
BIODB.COMPOUND.COMP   <- 'compoundcomp'

# Chromatographic column
BIODB.CHROM.COL      <- 'chromcol'
BIODB.CHROM.COL.NAME <- 'chrom.col.name'
BIODB.CHROM.COL.ID <- 'chrom.col.id'
BIODB.CHROM.COL.CONSTRUCTOR <- 'chrom.col.constructor'
BIODB.CHROM.COL.LENGTH <- 'chrom.col.length'
BIODB.CHROM.COL.DIAMETER <- 'chrom.col.diameter'
BIODB.CHROM.COL.RT  <- 'chromcolrt'
BIODB.CHROM.COL.RT.MIN  <- 'chrom.col.rt.min'
BIODB.CHROM.COL.RT.MAX  <- 'chrom.col.rt.max'
BIODB.CHROM.COL.METHOD.PROTOCOL <- 'chrom.col.method.protocol'

BIODB.ID <- 'id'
BIODB.TITLE <- 'title'
BIODB.PEAK.MZ <- 'peak.mz'
BIODB.PEAK.RT <- 'peak.rt'
BIODB.PEAK.MZEXP <- 'peak.mzexp'
BIODB.PEAK.MZTHEO <- 'peak.mztheo'
BIODB.PEAK.FORMULA <- 'peak.formula'
BIODB.PEAK.FORMULA.COUNT <- 'peak.formula.count'
BIODB.PEAK.COMP     <- 'peak.comp' # Peak composition
BIODB.PEAK.ATTR     <- 'peak.attr' # Peak attribution
BIODB.PEAK.MASS <- 'peak.mass'
#	BIODB.PEAK.ATTR <- 'attr'
BIODB.PEAK.ERROR.PPM <- 'peak.error.ppm'
BIODB.PEAK.INTENSITY <- 'peak.intensity'
BIODB.PEAK.RELATIVE.INTENSITY <- 'peak.relative.intensity'
BIODB.CATALYTIC.ACTIVITY <- 'catalytic.activity'
BIODB.COFACTOR <- 'cofactor'
BIODB.CHARGE <- 'charge'

MULTIVAL.FIELD.SEP <- ';'

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
BIODB.FIELD.COMPUTING[[BIODB.INCHI]]      <- c(BIODB.CHEBI)
BIODB.FIELD.COMPUTING[[BIODB.INCHIKEY]]   <- c(BIODB.CHEBI)
BIODB.FIELD.COMPUTING[[BIODB.SEQUENCE]]   <- c(BIODB.NCBI.CCDS)

####################
# PEAKS DATA FRAME #
####################

# Peak fields

BIODB.PEAK.FIELDS <- c(BIODB.PEAK.MZ, BIODB.PEAK.INTENSITY, BIODB.PEAK.RELATIVE.INTENSITY, BIODB.PEAK.FORMULA, BIODB.PEAK.FORMULA.COUNT, BIODB.PEAK.MASS, BIODB.PEAK.ERROR.PPM, BIODB.PEAK.MZEXP, BIODB.PEAK.MZTHEO, BIODB.PEAK.COMP, BIODB.PEAK.ATTR)

# Example
BIODB.PEAK.DF.EXAMPLE <- data.frame(mz = double(), int = double(), rel.int = integer(), formula = character(), formula.count <- integer(), mass = double(), error = double(), stringsAsFactors = FALSE)
colnames(BIODB.PEAK.DF.EXAMPLE) <- c(BIODB.PEAK.MZ, BIODB.PEAK.INTENSITY, BIODB.PEAK.RELATIVE.INTENSITY, BIODB.PEAK.FORMULA, BIODB.PEAK.FORMULA.COUNT, BIODB.PEAK.MASS, BIODB.PEAK.ERROR.PPM)
