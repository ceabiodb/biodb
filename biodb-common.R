if ( ! exists('BIODB.COMPOUND')) { # Do not load again if already loaded

	###############
	# ENTRY TYPES #
	###############

	BIODB.COMPOUND <- 'compound'
	BIODB.SPECTRUM <- 'spectrum'
	
	#######################
	# ENTRY CONTENT TYPES #
	#######################

	BIODB.HTML <- 'html'
	BIODB.TXT  <- 'txt'
	BIODB.XML  <- 'xml'
	BIODB.CSV  <- 'csv'
	BIODB.DATAFRAME  <- 'dataframe'
	BIODB.ANY  <- 'any' # Value used when we do not care about the type.

	#############
	# DATABASES #
	#############

	BIODB.CHEBI        <- 'chebi'
	BIODB.KEGG         <- 'kegg'
	BIODB.PUBCHEMCOMP  <- 'pubchemcomp' # Compound database
	BIODB.PUBCHEMSUB   <- 'pubchemsub'  # Substance database
	BIODB.HMDB         <- 'hmdb'
	BIODB.CHEMSPIDER   <- 'chemspider'
	BIODB.ENZYME       <- 'enzyme'
	BIODB.LIPIDMAPS    <- 'lipidmaps'
	BIODB.MIRBASE      <- 'mirbase'
	BIODB.NCBIGENE     <- 'ncbigene'
	BIODB.NCBICCDS     <- 'ncbiccds'
	BIODB.UNIPROT      <- 'uniprot'
	BIODB.MASSBANK     <- 'massbank'
	BIODB.MASSFILEDB   <- 'massfiledb'

	BIODB.ONLINE.DATABASES <- c(BIODB.CHEBI, BIODB.KEGG, BIODB.PUBCHEMCOMP, BIODB.PUBCHEMSUB, BIODB.HMDB, BIODB.CHEMSPIDER, BIODB.ENZYME, BIODB.LIPIDMAPS, BIODB.MIRBASE, BIODB.NCBIGENE, BIODB.NCBICCDS, BIODB.UNIPROT, BIODB.MASSBANK)

	##########
	# FIELDS #
	##########

	BIODB.ACCESSION    <- 'accession'
	BIODB.DESCRIPTION  <- 'description'
	BIODB.PROTEIN.DESCRIPTION  <- 'protdesc'
	BIODB.NAME         <- 'name'
	BIODB.FULLNAMES    <- 'fullnames'
	BIODB.SYNONYMS     <- 'synonyms'
	BIODB.SYMBOL       <- 'symbol'
	BIODB.GENE.SYMBOLS <- 'genesymbols'
	BIODB.CHEBI.ID     <- 'chebiid'
	BIODB.LIPIDMAPS.ID <- 'lipidmapsid'
	BIODB.KEGG.ID      <- 'keggid'
	BIODB.HMDB.ID      <- 'hmdbid'
	BIODB.ENZYME.ID    <- 'enzymeid'
	BIODB.NCBI.CCDS.ID <- 'ncbiccdsid'
	BIODB.NCBI.GENE.ID <- 'ncbigeneid'
	BIODB.PUBCHEMCOMP.ID   <- 'pubchemcompid'
	BIODB.PUBCHEMSUB.ID   <- 'pubchemsubid'
	BIODB.CHEMSPIDER.ID   <- 'chemspiderid'
	BIODB.UNIPROT.ID   <- 'uniprotid'
	BIODB.CAS.ID        <- 'casid'
	BIODB.SMILES        <- 'smiles'
	BIODB.INCHI        <- 'inchi'
	BIODB.INCHIKEY     <- 'inchikey'
	BIODB.MSDEV        <- 'msdev'
	BIODB.MSDEVTYPE    <- 'msdevtype'
	BIODB.MSTYPE       <- 'mstype'
	BIODB.MSMODE       <- 'msmode'
	BIODB.MSPRECMZ     <- 'msprecmz'       # numeric
	BIODB.MSPRECANNOT  <- 'msprecannot'
	BIODB.FORMULA      <- 'formula'
	BIODB.SUPER.CLASS  <- 'superclass'
	BIODB.MASS         <- 'mass'
	BIODB.AVERAGE.MASS <- 'averagemass'
	BIODB.MONOISOTOPIC.MASS <- 'monoisotopicmass'
	BIODB.SEQUENCE     <- 'sequence'
	BIODB.LOCATION     <- 'location'
	BIODB.LENGTH       <- 'length'
	BIODB.NB.PEAKS     <- 'nbpeaks'
	BIODB.PEAKS        <- 'peaks'
	BIODB.COMPOUND.ID   <- 'compoundid'
	BIODB.COMPOUND.MASS   <- 'compoundmass'
	BIODB.COMPOUND.COMP   <- 'compoundcomp'
	BIODB.CHROM.COL     <- 'chromcol' # Chromatographic column
	BIODB.CHROM.COL.RT  <- 'chromcolrt' # Retention time measured on chromatographic column
	BIODB.ID <- 'id'
	BIODB.TITLE <- 'title'

	# Mode values
	BIODB.MSMODE.NEG <- 'neg'
	BIODB.MSMODE.POS <- 'pos'

	#################
	# CARDINALITIES #
	#################

	BIODB.CARD.ONE <- '1'
	BIODB.CARD.MANY <- '*'

	##########################
	# ENTRY FIELD ATTRIBUTES #
	##########################

	BIODB.FIELDS <- data.frame(matrix(c(
		# FIELD NAME                CLASS           CARDINALITY
		BIODB.COMPOUND,             'BiodEntry',    BIODB.CARD.ONE,
		BIODB.ACCESSION,            'character',    BIODB.CARD.ONE,
		BIODB.DESCRIPTION,          'character',    BIODB.CARD.ONE,
		BIODB.NAME,                 'character',    BIODB.CARD.ONE,
		BIODB.FULLNAMES,            'character',    BIODB.CARD.MANY,
		BIODB.SYNONYMS,             'character',    BIODB.CARD.MANY,
		BIODB.PROTEIN.DESCRIPTION,  'character',    BIODB.CARD.ONE,
		BIODB.SYMBOL,               'character',    BIODB.CARD.ONE,
		BIODB.GENE.SYMBOLS,         'character',    BIODB.CARD.MANY,
		BIODB.CHEBI.ID,             'character',    BIODB.CARD.ONE,
		BIODB.LIPIDMAPS.ID,         'character',    BIODB.CARD.ONE,
		BIODB.KEGG.ID,              'character',    BIODB.CARD.ONE,
		BIODB.HMDB.ID,              'character',    BIODB.CARD.ONE,
		BIODB.ENZYME.ID,            'character',    BIODB.CARD.ONE,
		BIODB.PUBCHEMCOMP.ID,       'character',    BIODB.CARD.ONE,
		BIODB.PUBCHEMSUB.ID,        'character',    BIODB.CARD.ONE,
		BIODB.UNIPROT.ID,           'character',    BIODB.CARD.ONE,
		BIODB.NCBI.CCDS.ID,         'character',    BIODB.CARD.ONE,
		BIODB.NCBI.GENE.ID,         'character',    BIODB.CARD.ONE,
		BIODB.INCHI,                'character',    BIODB.CARD.ONE,
		BIODB.INCHIKEY,             'character',    BIODB.CARD.ONE,
		BIODB.MSDEV,                'character',    BIODB.CARD.ONE,
		BIODB.MSDEVTYPE,            'character',    BIODB.CARD.ONE,
		BIODB.MSTYPE,               'character',    BIODB.CARD.ONE,
		BIODB.MSMODE,               'character',    BIODB.CARD.ONE,
		BIODB.MSPRECMZ,             'double',       BIODB.CARD.ONE,
		BIODB.MSPRECANNOT,          'character',    BIODB.CARD.ONE,
		BIODB.FORMULA,              'character',    BIODB.CARD.ONE,
		BIODB.SUPER.CLASS,          'character',    BIODB.CARD.ONE,
		BIODB.MASS,                 'double',       BIODB.CARD.ONE,
		BIODB.AVERAGE.MASS,         'double',       BIODB.CARD.ONE,
		BIODB.MONOISOTOPIC.MASS,    'double',       BIODB.CARD.ONE,
		BIODB.SEQUENCE,             'character',    BIODB.CARD.ONE,
		BIODB.LENGTH,               'integer',      BIODB.CARD.ONE,
		BIODB.LOCATION,             'character',    BIODB.CARD.ONE,
		BIODB.NB.PEAKS,             'integer',      BIODB.CARD.ONE,
		BIODB.PEAKS,                'data.frame',   BIODB.CARD.ONE,
		BIODB.SMILES,               'character',    BIODB.CARD.ONE,
		BIODB.CHEMSPIDER.ID,        'character',    BIODB.CARD.ONE,
		BIODB.CAS.ID,               'character',    BIODB.CARD.ONE
		), byrow = TRUE, ncol = 3), stringsAsFactors = FALSE)
	colnames(BIODB.FIELDS) <- c('name', 'class', 'cardinality')

	#########################
	# GET DATABASE ID FIELD #
	#########################

	biodb.get.database.id.field <- function(database) {

		id.field <- NA_character_

		if (database %in% BIODB.ONLINE.DATABASES) {
			id.field <- paste0(database, 'id')
			if ( ! id.field %in% BIODB.FIELDS[['name']])
				stop(paste0('No ID field defined for database ', database, '.'))
		}

		return(id.field)
	}

	#####################
	# COMPUTABLE FIELDS #
	#####################

	BIODB.FIELD.COMPUTING <- list()
	BIODB.FIELD.COMPUTING[[BIODB.INCHI]]      <- c(BIODB.CHEBI)
	BIODB.FIELD.COMPUTING[[BIODB.INCHIKEY]]   <- c(BIODB.CHEBI)
	BIODB.FIELD.COMPUTING[[BIODB.SEQUENCE]]   <- c(BIODB.NCBICCDS)

	####################
	# PEAKS DATA FRAME #
	####################

	# Columns
	BIODB.PEAK.MZ <- 'mz'
	BIODB.PEAK.RT <- 'rt'
	BIODB.PEAK.MZEXP <- 'mzexp'
	BIODB.PEAK.MZTHEO <- 'mztheo'
	BIODB.PEAK.FORMULA <- 'formula'
	BIODB.PEAK.FORMULA.COUNT <- 'formula.count'
	BIODB.PEAK.COMP     <- 'peakcomp' # Peak composition
	BIODB.PEAK.ATTR     <- 'peakattr' # Peak attribution
	BIODB.PEAK.MASS <- 'mass'
#	BIODB.PEAK.ATTR <- 'attr'
	BIODB.PEAK.ERROR.PPM <- 'error.ppm'
	BIODB.PEAK.INTENSITY <- 'intensity'
	BIODB.PEAK.RELATIVE.INTENSITY <- 'relative.intensity'

	# Example
	BIODB.PEAK.DF.EXAMPLE <- data.frame(mz = double(), int = double(), rel.int = integer(), formula = character(), formula.count <- integer(), mass = double(), error = double(), stringsAsFactors = FALSE)
	colnames(BIODB.PEAK.DF.EXAMPLE) <- c(BIODB.PEAK.MZ, BIODB.PEAK.INTENSITY, BIODB.PEAK.RELATIVE.INTENSITY, BIODB.PEAK.FORMULA, BIODB.PEAK.FORMULA.COUNT, BIODB.PEAK.MASS, BIODB.PEAK.ERROR.PPM)

	#################
	# GET ENTRY URL #
	#################

	# TODO Let the choice to use either jp or eu
	BIODB.MASSBANK.JP.WS.URL  <- "http://www.massbank.jp/api/services/MassBankAPI/"
	BIODB.MASSBANK.EU.WS.URL  <- "http://massbank.eu/api/services/MassBankAPI/"

	.do.get.entry.url <- function(class, accession, content.type = BIODB.ANY, base.url = NA_character_, token = NA_character_) {

		# TODO Only Massbank can handle multiple accession ids
		if (class != 'massbank' && length(accession) > 1)
			stop(paste0("Cannot build a URL for getting multiple entries for class ", class, "."))

		url <- switch(class,
			chebi       = if (content.type %in% c(BIODB.ANY, BIODB.HTML)) paste0('https://www.ebi.ac.uk/chebi/searchId.do?chebiId=', accession) else NULL,
			chemspider  = {
							token.param <- if (is.na(token)) '' else paste0('&token', token),
							switch(if (content.type == BIODB.ANY) BIODB.XML else content.type,
			                       html = paste0('http://www.chemspider.com/Chemical-Structure.', accession, '.html'),
							       xml = paste0('http://www.chemspider.com/MassSpecAPI.asmx/GetExtendedCompoundInfoArray?', paste(paste0('CSIDs=', accession), collapse = '&'), token.param),
		                           NULL)
			},
			enzyme      = if (content.type %in% c(BIODB.ANY, BIODB.TXT)) paste0('http://enzyme.expasy.org/EC/', accession, '.txt') else NULL,
			hmdb        = switch(content.type,
			                     xml = paste0('http://www.hmdb.ca/metabolites/', accession, '.xml'),
			                     html = paste0('http://www.hmdb.ca/metabolites/', accession),
			                     any = paste0('http://www.hmdb.ca/metabolites/', accession),
			                     NULL),
			kegg        = switch(content.type,
			                     txt = paste0('http://rest.kegg.jp/get/', accession),
			                     html = paste0('http://www.genome.jp/dbget-bin/www_bget?cpd:', accession),
			                     any  = paste0('http://www.genome.jp/dbget-bin/www_bget?cpd:', accession),
			                     NULL),
			lipidmaps   = if (content.type %in% c(BIODB.ANY, BIODB.CSV)) paste0('http://www.lipidmaps.org/data/LMSDRecord.php?Mode=File&LMID=', accession, '&OutputType=CSV&OutputQuote=No') else NULL, 
			massbank    = if (content.type %in% c(BIODB.ANY, BIODB.TXT)) paste0((if (is.na(base.url)) BIODB.MASSBANK.EU.WS.URL else base.url), 'getRecordInfo?ids=', paste(accession, collapse = ',')) else NULL,
			mirbase     = if (content.type %in% c(BIODB.ANY, BIODB.HTML)) paste0('http://www.mirbase.org/cgi-bin/mature.pl?mature_acc=', accession) else NULL,
			pubchemcomp = switch(content.type,
			                     xml = paste0('http://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/compound/', accession, '/XML'),
			                     html = paste0('http://pubchem.ncbi.nlm.nih.gov/compound/', accession),
			                     NULL),
			pubchemsub  = switch(content.type,
			                     xml = paste0('http://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/substance/', accession, '/XML'),
			                     html = paste0('http://pubchem.ncbi.nlm.nih.gov/substance/', accession),
			                     NULL),
			ncbigene    = if (content.type %in% c(BIODB.ANY, BIODB.XML)) paste0('http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=gene&id=', accession, '&rettype=xml&retmode=text') else NULL,
			ncbiccds    = if (content.type %in% c(BIODB.ANY, BIODB.HTML)) paste0('https://www.ncbi.nlm.nih.gov/CCDS/CcdsBrowse.cgi?REQUEST=CCDS&GO=MainBrowse&DATA=', accession),
			uniprot     = if (content.type %in% c(BIODB.ANY, BIODB.XML)) paste0('http://www.uniprot.org/uniprot/', accession, '.xml'),
			NULL
			)

		return(url)
	}

	get.entry.url <- function(class, accession, content.type = BIODB.ANY, max.length = 0, base.url = NA_character_) {

		if (length(accession) == 0)
			return(NULL)

		full.url <- .do.get.entry.url(class, accession, content.type = content.type, base.url = base.url)
		if (max.length == 0 || nchar(full.url) <= max.length)
			return(if (max.length == 0) full.url else list(url = full.url, n = length(accession)))

		# Find max size URL
		a <- 1
		b <- length(accession)
		while (a < b) {
			m <- as.integer((a + b) / 2)
			url <- .do.get.entry.url(class, accession[1:m], content.type = content.type)
			if (nchar(url) <= max.length && m != a)
				a <- m
			else
				b <- m
		}
		url <- .do.get.entry.url(class, accession[1:a], content.type = content.type)
			
		return(list( url = url, n = a))
	}

	#################
	# PRINT MESSAGE #
	#################

	BIODB.DEBUG <- 1
	BIODB.LEVEL.NAMES <- c('DEBUG')

	.print.msg <- function(msg, level = BIODB.DEBUG, class = NA_character_) {
		cat(paste0(BIODB.LEVEL.NAMES[[level]], if (is.na(class)) '' else paste0(", ", class), ": ", msg, "\n"), file = stderr())
	}

	#####################
	# GET BIODB ENV VAR #
	#####################

	.get.biodb.env.var <- function(v) {

		# Get all env vars
		env <- Sys.getenv()

		# Make env var name
		env.var <- paste(c('BIODB', toupper(v)), collapse = '_')

		# Look if this env var exists
		if (env.var %in% names(env))
			return(env[[env.var]])

		return(NA_character_)
	}
}
