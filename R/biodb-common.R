if ( ! exists('BIODB.XML')) {

	###############
	# CACHE MODES #
	###############

	BIODB.CACHE.READ.ONLY  <- 'read-only'
	BIODB.CACHE.READ.WRITE <- 'read-write'
	BIODB.CACHE.WRITE.ONLY <- 'write-only'

	#######################
	# ENTRY CONTENT TYPES #
	#######################

	BIODB.HTML <- 'html'
	BIODB.TXT  <- 'txt'
	BIODB.XML  <- 'xml'
	BIODB.CSV  <- 'csv'
	BIODB.DATAFRAME  <- 'dataframe'
	BIODB.JSON <- 'json'

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
	BIODB.PEAKFOREST   <- 'peakforest'

	BIODB.DATABASES <- c(BIODB.CHEBI, BIODB.KEGG, BIODB.PUBCHEMCOMP, BIODB.PUBCHEMSUB, BIODB.HMDB, BIODB.CHEMSPIDER, BIODB.ENZYME, BIODB.LIPIDMAPS, BIODB.MIRBASE, BIODB.NCBIGENE, BIODB.NCBICCDS, BIODB.UNIPROT, BIODB.MASSBANK, BIODB.MASSFILEDB, BIODB.PEAKFOREST)

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
	BIODB.PEAKFOREST.ID <- 'peakforestid'
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
	BIODB.COMPOUNDS    <- 'compounds'
    BIODB.NB.COMPOUNDS <- 'nbcompounds'
	BIODB.COMPOUND.ID   <- 'compoundid'
	BIODB.COMPOUND.MASS   <- 'compoundmass'
	BIODB.COMPOUND.COMP   <- 'compoundcomp'
	BIODB.CHROM.COL     <- 'chromcol' # Chromatographic column
	BIODB.CHROM.COL.RT  <- 'chromcolrt' # Retention time measured on chromatographic column
	BIODB.ID <- 'id'
	BIODB.TITLE <- 'title'
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

	# Mode values
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
	BIODB.MSMS.DIST <- c(BIODB.MSMS.DIST.COS, BIODB.MSMS.DIST.WCOSINE, BIODB.MSMS.DIST.PKERNEL)
	
	
	#################
	# CARDINALITIES #
	#################

	BIODB.CARD.ONE <- '1'
	BIODB.CARD.MANY <- '*'
	
	#####################
	#INTENSITy NOTATIONS#
	#####################
	
	BIODB.GROUP.INTENSITY<-c(BIODB.PEAK.INTENSITY,BIODB.PEAK.RELATIVE.INTENSITY)

	##########################
	# ENTRY FIELD ATTRIBUTES #
	##########################
	# FIELD NAME                CLASS           CARDINALITY			TYPE  
	BIODB.FIELDS <- data.frame(matrix(c(
		BIODB.ACCESSION,            'character',    BIODB.CARD.ONE,		'none',
		BIODB.DESCRIPTION,          'character',    BIODB.CARD.ONE,		'none',
		BIODB.NAME,                 'character',    BIODB.CARD.ONE,		'name',
		BIODB.COMP.IUPAC.NAME.ALLOWED,	'character',    BIODB.CARD.ONE,		'name',
		BIODB.COMP.IUPAC.NAME.TRAD,    	'character',    BIODB.CARD.ONE,		'name',
		BIODB.COMP.IUPAC.NAME.SYST,    	'character',    BIODB.CARD.ONE,		'name',
		BIODB.COMP.IUPAC.NAME.PREF,    	'character',    BIODB.CARD.ONE,		'name',
		BIODB.COMP.IUPAC.NAME.CAS,    	'character',    BIODB.CARD.ONE,		'name',
		BIODB.FULLNAMES,            'character',    BIODB.CARD.MANY,	'name',
		BIODB.SYNONYMS,             'character',    BIODB.CARD.MANY,	'name',
		BIODB.PROTEIN.DESCRIPTION,  'character',    BIODB.CARD.ONE,		'none',
		BIODB.SYMBOL,               'character',    BIODB.CARD.ONE,		'none',
		BIODB.GENE.SYMBOLS,         'character',    BIODB.CARD.MANY,	'none',
		BIODB.NB.COMPOUNDS,         'integer',      BIODB.CARD.ONE,     'none',
		BIODB.COMPOUNDS,            'object',       BIODB.CARD.MANY,  'none',
		BIODB.CHEBI.ID,             'character',    BIODB.CARD.ONE,		'none',
		BIODB.LIPIDMAPS.ID,         'character',    BIODB.CARD.ONE,		'none',
		BIODB.KEGG.ID,              'character',    BIODB.CARD.ONE,		'none',
		BIODB.HMDB.ID,              'character',    BIODB.CARD.ONE,		'none',
		BIODB.ENZYME.ID,            'character',    BIODB.CARD.ONE,		'none',
		BIODB.PUBCHEMCOMP.ID,       'character',    BIODB.CARD.ONE,		'none',
		BIODB.PUBCHEMSUB.ID,        'character',    BIODB.CARD.ONE,		'none',
		BIODB.PEAKFOREST.ID,        'character',    BIODB.CARD.ONE,		'none',
		BIODB.UNIPROT.ID,           'character',    BIODB.CARD.ONE,		'none',
		BIODB.NCBI.CCDS.ID,         'character',    BIODB.CARD.ONE,		'none',
		BIODB.NCBI.GENE.ID,         'character',    BIODB.CARD.ONE,		'none',
		BIODB.INCHI,                'character',    BIODB.CARD.ONE,		'none',
		BIODB.INCHIKEY,             'character',    BIODB.CARD.ONE,		'none',
		BIODB.MSDEV,                'character',    BIODB.CARD.ONE,		'none',
		BIODB.MSDEVTYPE,            'character',    BIODB.CARD.ONE,		'none',
		BIODB.MSTYPE,               'character',    BIODB.CARD.ONE,		'none',
		BIODB.MSMODE,               'character',    BIODB.CARD.ONE,		'none',
		BIODB.MSPRECMZ,             'double',       BIODB.CARD.ONE,		'none',
		BIODB.PEAK.MZTHEO,			'double',		BIODB.CARD.ONE,		'none',
		BIODB.MSPRECANNOT,          'character',    BIODB.CARD.ONE,		'none',
		BIODB.FORMULA,              'character',    BIODB.CARD.ONE,		'none',
		BIODB.SUPER.CLASS,          'character',    BIODB.CARD.ONE,		'none',
		BIODB.MASS,                 'double',       BIODB.CARD.ONE,		'none',
		BIODB.AVERAGE.MASS,         'double',       BIODB.CARD.ONE,		'none',
		BIODB.MONOISOTOPIC.MASS,    'double',       BIODB.CARD.ONE,		'none',
		BIODB.SEQUENCE,             'character',    BIODB.CARD.ONE,		'none',
		BIODB.LENGTH,               'integer',      BIODB.CARD.ONE,		'none',
		BIODB.LOCATION,             'character',    BIODB.CARD.ONE,		'none',
		BIODB.NB.PEAKS,             'integer',      BIODB.CARD.ONE,		'none',
		BIODB.PEAKS,                'data.frame',   BIODB.CARD.ONE,		'none',
		BIODB.SMILES,               'character',    BIODB.CARD.ONE,		'none',
		BIODB.CHEMSPIDER.ID,        'character',    BIODB.CARD.ONE,		'none',
		BIODB.CAS.ID,               'character',    BIODB.CARD.ONE,		'none'
		), byrow = TRUE, ncol = 4), stringsAsFactors = FALSE)
	colnames(BIODB.FIELDS) <- c('name', 'class', 'cardinality', 'type')

	#########################
	# GET DATABASE ID FIELD #
	#########################

	biodb.get.database.id.field <- function(database) {

		id.field <- NA_character_

		if (database %in% BIODB.DATABASES) {
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

	# Example
	BIODB.PEAK.DF.EXAMPLE <- data.frame(mz = double(), int = double(), rel.int = integer(), formula = character(), formula.count <- integer(), mass = double(), error = double(), stringsAsFactors = FALSE)
	colnames(BIODB.PEAK.DF.EXAMPLE) <- c(BIODB.PEAK.MZ, BIODB.PEAK.INTENSITY, BIODB.PEAK.RELATIVE.INTENSITY, BIODB.PEAK.FORMULA, BIODB.PEAK.FORMULA.COUNT, BIODB.PEAK.MASS, BIODB.PEAK.ERROR.PPM)

	#################
	# GET ENTRY URL #
	#################

	# TODO Let the choice to use either jp or eu
	BIODB.MASSBANK.JP.WS.URL  <- "http://www.massbank.jp/api/services/MassBankAPI/"
	BIODB.MASSBANK.EU.WS.URL  <- "http://massbank.eu/api/services/MassBankAPI/"

	.do.get.entry.url <- function(class, accession, content.type = BIODB.HTML, base.url = NA_character_, token = NA_character_) {

		# Only certain databases can handle multiple accession ids
		if ( ! class %in% c(BIODB.MASSBANK, BIODB.CHEMSPIDER, BIODB.PUBCHEMCOMP, BIODB.PUBCHEMSUB, BIODB.PEAKFOREST) && length(accession) > 1)
			stop(paste0("Cannot build a URL for getting multiple entries for class ", class, "."))

		# Get URL
		url <- switch(class,
			chebi       = if (content.type == BIODB.HTML) paste0('https://www.ebi.ac.uk/chebi/searchId.do?chebiId=', accession) else NULL,
			chemspider  = {
							token.param <- if (is.na(token)) '' else paste('&token', token, sep = '=')
							switch(content.type,
			                       html = paste0('http://www.chemspider.com/Chemical-Structure.', accession, '.html'),
							       xml = paste0('http://www.chemspider.com/MassSpecAPI.asmx/GetExtendedCompoundInfoArray?', paste(paste0('CSIDs=', accession), collapse = '&'), token.param),
		                           NULL)
			},
			enzyme      = if (content.type == BIODB.TXT) paste0('http://enzyme.expasy.org/EC/', accession, '.txt') else NULL,
			hmdb        = switch(content.type,
			                     xml = paste0('http://www.hmdb.ca/metabolites/', accession, '.xml'),
			                     html = paste0('http://www.hmdb.ca/metabolites/', accession),
			                     NULL),
			kegg        = switch(content.type,
			                     txt = paste0('http://rest.kegg.jp/get/', accession),
			                     html = paste0('http://www.genome.jp/dbget-bin/www_bget?cpd:', accession),
			                     NULL),
			lipidmaps   = if (content.type == BIODB.CSV) paste0('http://www.lipidmaps.org/data/LMSDRecord.php?Mode=File&LMID=', accession, '&OutputType=CSV&OutputQuote=No') else NULL, 
			massbank    = if (content.type == BIODB.TXT) paste0((if (is.na(base.url)) BIODB.MASSBANK.EU.WS.URL else base.url), 'getRecordInfo?ids=', paste(accession, collapse = ',')) else NULL,
			mirbase     = if (content.type == BIODB.HTML) paste0('http://www.mirbase.org/cgi-bin/mature.pl?mature_acc=', accession) else NULL,
			pubchemcomp = switch(content.type,
			                     xml = paste0('https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/', paste(accession, collapse = ','), '/XML'),
			                     html = paste0('http://pubchem.ncbi.nlm.nih.gov/compound/', accession),
			                     NULL),
			pubchemsub  = switch(content.type,
			                     xml = paste0('https://pubchem.ncbi.nlm.nih.gov/rest/pug/substance/sid/', paste(accession, collapse = ','), '/XML'),
			                     html = paste0('http://pubchem.ncbi.nlm.nih.gov/substance/', accession),
			                     NULL),
			ncbigene    = if (content.type == BIODB.XML) paste0('https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=gene&id=', accession, '&rettype=xml&retmode=text') else NULL,
			ncbiccds    = if (content.type == BIODB.HTML) paste0('https://www.ncbi.nlm.nih.gov/CCDS/CcdsBrowse.cgi?REQUEST=CCDS&GO=MainBrowse&DATA=', accession),
			uniprot     = if (content.type == BIODB.XML) paste0('http://www.uniprot.org/uniprot/', accession, '.xml'),
			peakforest  = switch(content.type,
			                     html= paste0('https://peakforest.org/home?PFs=',accession),
			                     json= paste0('https://peakforest-alpha.inra.fr/rest/spectra/lcms/ids/',paste(accession,sep=','),'?token=',token),
			                     
			NULL
			)
		)
		return(url)
	}

	get.entry.url <- function(class, accession, content.type = BIODB.HTML, max.length = 0, base.url = NA_character_, token = NA_character_) {

		if (length(accession) == 0)
			return(NULL)

		full.url <- .do.get.entry.url(class, accession, content.type = content.type, base.url = base.url, token = token)
		if (max.length == 0 || nchar(full.url) <= max.length)
			return(if (max.length == 0) full.url else list(url = full.url, n = length(accession)))

		# Find max size URL
		a <- 1
		b <- length(accession)
		while (a < b) {
			m <- as.integer((a + b) / 2)
			url <- .do.get.entry.url(class, accession[1:m], content.type = content.type, base.url = base.url, token = token)
			if (nchar(url) <= max.length && m != a)
				a <- m
			else
				b <- m
		}
		url <- .do.get.entry.url(class, accession[1:a], content.type = content.type, base.url = base.url, token = token)
			
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
	# BIODB GET ENV VAR #
	#####################

	.biodb.get.env.var <- function(v) {

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
