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
	BIODB.PUBCHEM      <- 'pubchem'
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
	BIODB.PUBCHEM.ID   <- 'pubchemid'
	BIODB.UNIPROT.ID   <- 'uniprotid'
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
	BIODB.PEAK.MZ       <- 'peakmz'
	BIODB.PEAK.COMP     <- 'peakcomp' # Peak composition
	BIODB.PEAK.ATTR     <- 'peakattr' # Peak attribution
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
		BIODB.COMPOUND,            'BiodEntry',    BIODB.CARD.ONE,
		BIODB.ACCESSION,           'character',    BIODB.CARD.ONE,
		BIODB.DESCRIPTION,         'character',    BIODB.CARD.ONE,
		BIODB.NAME,                'character',    BIODB.CARD.ONE,
		BIODB.FULLNAMES,           'character',    BIODB.CARD.MANY,
		BIODB.SYNONYMS,            'character',    BIODB.CARD.MANY,
		BIODB.PROTEIN.DESCRIPTION, 'character',    BIODB.CARD.ONE,
		BIODB.SYMBOL,              'character',    BIODB.CARD.ONE,
		BIODB.GENE.SYMBOLS,        'character',    BIODB.CARD.MANY,
		BIODB.CHEBI.ID,            'character',    BIODB.CARD.ONE,
		BIODB.LIPIDMAPS.ID,        'character',    BIODB.CARD.ONE,
		BIODB.KEGG.ID,             'character',    BIODB.CARD.ONE,
		BIODB.HMDB.ID,             'character',    BIODB.CARD.ONE,
		BIODB.ENZYME.ID,           'character',    BIODB.CARD.ONE,
		BIODB.PUBCHEM.ID,          'character',    BIODB.CARD.ONE,
		BIODB.UNIPROT.ID,          'character',    BIODB.CARD.ONE,
		BIODB.NCBI.CCDS.ID,        'character',    BIODB.CARD.ONE,
		BIODB.NCBI.GENE.ID,        'character',    BIODB.CARD.ONE,
		BIODB.INCHI,               'character',    BIODB.CARD.ONE,
		BIODB.INCHIKEY,            'character',    BIODB.CARD.ONE,
		BIODB.MSDEV,               'character',    BIODB.CARD.ONE,
		BIODB.MSDEVTYPE,           'character',    BIODB.CARD.ONE,
		BIODB.MSTYPE,              'character',    BIODB.CARD.ONE,
		BIODB.MSMODE,              'character',    BIODB.CARD.ONE,
		BIODB.MSPRECMZ,            'double',       BIODB.CARD.ONE,
		BIODB.MSPRECANNOT,         'character',    BIODB.CARD.ONE,
		BIODB.FORMULA,             'character',    BIODB.CARD.ONE,
		BIODB.SUPER.CLASS,         'character',    BIODB.CARD.ONE,
		BIODB.MASS,                'double',       BIODB.CARD.ONE,
		BIODB.AVERAGE.MASS,        'double',       BIODB.CARD.ONE,
		BIODB.MONOISOTOPIC.MASS,   'double',       BIODB.CARD.ONE,
		BIODB.SEQUENCE,            'character',    BIODB.CARD.ONE,
		BIODB.LENGTH,              'integer',      BIODB.CARD.ONE,
		BIODB.LOCATION,            'character',    BIODB.CARD.ONE,
		BIODB.NB.PEAKS,            'integer',      BIODB.CARD.ONE,
		BIODB.PEAKS,               'data.frame',   BIODB.CARD.ONE
		), byrow = TRUE, ncol = 3), stringsAsFactors = FALSE)
	colnames(BIODB.FIELDS) <- c('name', 'class', 'cardinality')

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
	BIODB.PEAK.FORMULA <- 'formula'
	BIODB.PEAK.FORMULA.COUNT <- 'formula.count'
	BIODB.PEAK.MASS <- 'mass'
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
	BIODB.MASSBANK.JP.WS.URL  <- "http://www.massbank.jp/api/services/MassBankAPI/getRecordInfo"
	BIODB.MASSBANK.EU.WS.URL  <- "http://massbank.eu/api/services/MassBankAPI/getRecordInfo"

	get.entry.url <- function(class, accession, content.type = BIODB.ANY) {

		url <- switch(class,
			chebi       = if (content.type %in% c(BIODB.ANY, BIODB.HTML)) paste0('https://www.ebi.ac.uk/chebi/searchId.do?chebiId=', accession) else NULL,
			chemspider  = if (content.type %in% c(BIODB.ANY, BIODB.HTML)) paste0('http://www.chemspider.com/Chemical-Structure.', accession, '.html') else NULL,
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
			massbank    = if (content.type %in% c(BIODB.ANY, BIODB.TXT)) paste0(BIODB.MASSBANK.EU.WS.URL, '?ids=', paste(accession, collapse = ',')) else NULL,
			mirbase     = if (content.type %in% c(BIODB.ANY, BIODB.HTML)) paste0('http://www.mirbase.org/cgi-bin/mature.pl?mature_acc=', accession) else NULL,
			pubchem     = {
							accession <- gsub(' ', '', accession, perl = TRUE)
							accession <- gsub('^CID', '', accession, perl = TRUE)
							switch(content.type,
			                     xml = paste0('http://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/compound/', accession, '/XML/?response_type=save&response_basename=CID_', accession),
			                     html = paste0('http://pubchem.ncbi.nlm.nih.gov/compound/', accession),
			                     NULL)
		    			  },
			ncbigene    = if (content.type %in% c(BIODB.ANY, BIODB.XML)) paste0('http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=gene&id=', accession, '&rettype=xml&retmode=text') else NULL,
			ncbiccds    = if (content.type %in% c(BIODB.ANY, BIODB.HTML)) paste0('https://www.ncbi.nlm.nih.gov/CCDS/CcdsBrowse.cgi?REQUEST=CCDS&GO=MainBrowse&DATA=', accession),
			uniprot     = if (content.type %in% c(BIODB.ANY, BIODB.XML)) paste0('http://www.uniprot.org/uniprot/', accession, '.xml'),
			NULL
			)

		return(url)
	}
}
