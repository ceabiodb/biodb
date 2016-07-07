if ( ! exists('RBIODB.COMPOUND')) { # Do not load again if already loaded

	#############
	# CONSTANTS #
	#############
	
	# Entry types
	RBIODB.COMPOUND <- 'compound'
	RBIODB.SPECTRUM <- 'spectrum'
	
	# Entry content types
	RBIODB.HTML <- 'html'
	RBIODB.TXT  <- 'txt'
	RBIODB.XML  <- 'xml'
	RBIODB.CSV  <- 'csv'
	RBIODB.ANY  <- 'any'

	# Class names
	RBIODB.CHEBI        <- 'chebi'
	RBIODB.KEGG         <- 'kegg'
	RBIODB.PUBCHEM      <- 'pubchem'
	RBIODB.HMDB         <- 'hmdb'
	RBIODB.CHEMSPIDER   <- 'chemspider'
	RBIODB.ENZYME       <- 'enzyme'
	RBIODB.LIPIDMAPS    <- 'lipidmaps'
	RBIODB.MIRBASE      <- 'mirbase'
	RBIODB.NCBIGENE     <- 'ncbigene'
	RBIODB.NCBICCDS     <- 'ncbiccds'
	RBIODB.UNIPROT      <- 'uniprot'
	RBIODB.MASSBANK     <- 'massbank'

	# Fields
	RBIODB.COMPOUND     <- 'compound'
	RBIODB.ACCESSION    <- 'accession'
	RBIODB.DESCRIPTION  <- 'description'
	RBIODB.PROTEIN.DESCRIPTION  <- 'protdesc'
	RBIODB.NAME         <- 'name'
	RBIODB.FULLNAMES    <- 'fullnames'
	RBIODB.SYNONYMS     <- 'synonyms'
	RBIODB.SYMBOL       <- 'symbol'
	RBIODB.GENE.SYMBOLS <- 'genesymbols'
	RBIODB.CHEBI.ID     <- 'chebiid'
	RBIODB.LIPIDMAPS.ID <- 'lipidmapsid'
	RBIODB.KEGG.ID      <- 'keggid'
	RBIODB.HMDB.ID      <- 'hmdbid'
	RBIODB.ENZYME.ID    <- 'enzymeid'
	RBIODB.NCBI.CCDS.ID <- 'ncbiccdsid'
	RBIODB.NCBI.GENE.ID <- 'ncbigeneid'
	RBIODB.PUBCHEM.ID   <- 'pubchemid'
	RBIODB.UNIPROT.ID   <- 'uniprotid'
	RBIODB.INCHI        <- 'inchi'
	RBIODB.INCHIKEY     <- 'inchikey'
	RBIODB.MSDEV        <- 'msdev'
	RBIODB.MSDEVTYPE    <- 'msdevtype'
	RBIODB.MSTYPE       <- 'mstype'
	RBIODB.MSMODE       <- 'msmode'
	RBIODB.MSPRECMZ     <- 'msprecmz'       # numeric
	RBIODB.MSPRECANNOT  <- 'msprecannot'
	RBIODB.FORMULA      <- 'formula'
	RBIODB.SUPER.CLASS  <- 'superclass'
	RBIODB.MASS         <- 'mass'
	RBIODB.AVERAGE.MASS <- 'averagemass'
	RBIODB.MONOISOTOPIC.MASS <- 'monoisotopicmass'
	RBIODB.SEQUENCE     <- 'sequence'
	RBIODB.LOCATION     <- 'location'
	RBIODB.LENGTH       <- 'length'
	RBIODB.NB.PEAKS     <- 'nbpeaks'
	RBIODB.NB.PEAKS     <- 'nbpeaks'
	RBIODB.PEAKS        <- 'peaks'

	# Mode values
	RBIODB.MSMODE.NEG <- 'neg'
	RBIODB.MSMODE.POS <- 'pos'

	# Cardinalities
	RBIODB.CARD.ONE <- '1'
	RBIODB.CARD.MANY <- '*'

	# Field attributes
	RBIODB.FIELDS <- data.frame(matrix(c(
		# FIELD NAME                CLASS           CARDINALITY
		RBIODB.COMPOUND,            'BiodEntry',    RBIODB.CARD.ONE,
		RBIODB.ACCESSION,           'character',    RBIODB.CARD.ONE,
		RBIODB.DESCRIPTION,         'character',    RBIODB.CARD.ONE,
		RBIODB.NAME,                'character',    RBIODB.CARD.ONE,
		RBIODB.FULLNAMES,           'character',    RBIODB.CARD.MANY,
		RBIODB.SYNONYMS,            'character',    RBIODB.CARD.MANY,
		RBIODB.PROTEIN.DESCRIPTION, 'character',    RBIODB.CARD.ONE,
		RBIODB.SYMBOL,              'character',    RBIODB.CARD.ONE,
		RBIODB.GENE.SYMBOLS,        'character',    RBIODB.CARD.MANY,
		RBIODB.CHEBI.ID,            'character',    RBIODB.CARD.ONE,
		RBIODB.LIPIDMAPS.ID,        'character',    RBIODB.CARD.ONE,
		RBIODB.KEGG.ID,             'character',    RBIODB.CARD.ONE,
		RBIODB.HMDB.ID,             'character',    RBIODB.CARD.ONE,
		RBIODB.ENZYME.ID,           'character',    RBIODB.CARD.ONE,
		RBIODB.PUBCHEM.ID,          'character',    RBIODB.CARD.ONE,
		RBIODB.UNIPROT.ID,          'character',    RBIODB.CARD.ONE,
		RBIODB.NCBI.CCDS.ID,        'character',    RBIODB.CARD.ONE,
		RBIODB.NCBI.GENE.ID,        'character',    RBIODB.CARD.ONE,
		RBIODB.INCHI,               'character',    RBIODB.CARD.ONE,
		RBIODB.INCHIKEY,            'character',    RBIODB.CARD.ONE,
		RBIODB.MSDEV,               'character',    RBIODB.CARD.ONE,
		RBIODB.MSDEVTYPE,           'character',    RBIODB.CARD.ONE,
		RBIODB.MSTYPE,              'character',    RBIODB.CARD.ONE,
		RBIODB.MSMODE,              'character',    RBIODB.CARD.ONE,
		RBIODB.MSPRECMZ,            'double',       RBIODB.CARD.ONE,
		RBIODB.MSPRECANNOT,         'character',    RBIODB.CARD.ONE,
		RBIODB.FORMULA,             'character',    RBIODB.CARD.ONE,
		RBIODB.SUPER.CLASS,         'character',    RBIODB.CARD.ONE,
		RBIODB.MASS,                'double',       RBIODB.CARD.ONE,
		RBIODB.AVERAGE.MASS,        'double',       RBIODB.CARD.ONE,
		RBIODB.MONOISOTOPIC.MASS,   'double',       RBIODB.CARD.ONE,
		RBIODB.SEQUENCE,            'character',    RBIODB.CARD.ONE,
		RBIODB.LENGTH,              'integer',      RBIODB.CARD.ONE,
		RBIODB.LOCATION,            'character',    RBIODB.CARD.ONE,
		RBIODB.NB.PEAKS,            'integer',      RBIODB.CARD.ONE,
		RBIODB.PEAKS,               'data.frame',   RBIODB.CARD.ONE
		), byrow = TRUE, ncol = 3), stringsAsFactors = FALSE)
	colnames(RBIODB.FIELDS) <- c('name', 'class', 'cardinality')

	# How to compute a missing field ?
	RBIODB.FIELD.COMPUTING <- list()
	RBIODB.FIELD.COMPUTING[[RBIODB.INCHI]]      <- c(RBIODB.CHEBI)
	RBIODB.FIELD.COMPUTING[[RBIODB.INCHIKEY]]   <- c(RBIODB.CHEBI)
	RBIODB.FIELD.COMPUTING[[RBIODB.SEQUENCE]]   <- c(RBIODB.NCBICCDS)

	# Peaks data frame columns
	RBIODB.PEAK.MZ <- 'mz'
	RBIODB.PEAK.FORMULA <- 'formula'
	RBIODB.PEAK.FORMULA.COUNT <- 'formula.count'
	RBIODB.PEAK.MASS <- 'mass'
	RBIODB.PEAK.ERROR.PPM <- 'error.ppm'
	RBIODB.PEAK.INTENSITY <- 'intensity'
	RBIODB.PEAK.RELATIVE.INTENSITY <- 'relative.intensity'
	RBIODB.PEAK.DF.EXAMPLE <- data.frame(mz = double(), int = double(), rel.int = integer(), formula = character(), formula.count <- integer(), mass = double(), error = double(), stringsAsFactors = FALSE)
	colnames(RBIODB.PEAK.DF.EXAMPLE) <- c(RBIODB.PEAK.MZ, RBIODB.PEAK.INTENSITY, RBIODB.PEAK.RELATIVE.INTENSITY, RBIODB.PEAK.FORMULA, RBIODB.PEAK.FORMULA.COUNT, RBIODB.PEAK.MASS, RBIODB.PEAK.ERROR.PPM)

	#################
	# GET ENTRY URL #
	#################

	# TODO Let the choice to use either jp or eu
	RBIODB.MASSBANK.JP.WS.URL  <- "http://www.massbank.jp/api/services/MassBankAPI/getRecordInfo"
	RBIODB.MASSBANK.EU.WS.URL  <- "http://massbank.eu/api/services/MassBankAPI/getRecordInfo"

	get.entry.url <- function(class, accession, content.type = RBIODB.ANY) {

		url <- switch(class,
			chebi       = if (content.type %in% c(RBIODB.ANY, RBIODB.HTML)) paste0('https://www.ebi.ac.uk/chebi/searchId.do?chebiId=', accession) else NULL,
			chemspider  = if (content.type %in% c(RBIODB.ANY, RBIODB.HTML)) paste0('http://www.chemspider.com/Chemical-Structure.', accession, '.html') else NULL,
			enzyme      = if (content.type %in% c(RBIODB.ANY, RBIODB.TXT)) paste0('http://enzyme.expasy.org/EC/', accession, '.txt') else NULL,
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
			lipidmaps   = if (content.type %in% c(RBIODB.ANY, RBIODB.CSV)) paste0('http://www.lipidmaps.org/data/LMSDRecord.php?Mode=File&LMID=', accession, '&OutputType=CSV&OutputQuote=No') else NULL, 
			massbank    = if (content.type %in% c(RBIODB.ANY, RBIODB.TXT)) paste0(RBIODB.MASSBANK.EU.WS.URL, '?ids=', paste(accession, collapse = ',')) else NULL,
			mirbase     = if (content.type %in% c(RBIODB.ANY, RBIODB.HTML)) paste0('http://www.mirbase.org/cgi-bin/mature.pl?mature_acc=', accession) else NULL,
			pubchem     = {
							accession <- gsub(' ', '', accession, perl = TRUE)
							accession <- gsub('^CID', '', accession, perl = TRUE)
							switch(content.type,
			                     xml = paste0('http://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/compound/', accession, '/XML/?response_type=save&response_basename=CID_', accession),
			                     html = paste0('http://pubchem.ncbi.nlm.nih.gov/compound/', accession),
			                     NULL)
		    			  },
			ncbigene    = if (content.type %in% c(RBIODB.ANY, RBIODB.XML)) paste0('http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=gene&id=', accession, '&rettype=xml&retmode=text') else NULL,
			ncbiccds    = if (content.type %in% c(RBIODB.ANY, RBIODB.HTML)) paste0('https://www.ncbi.nlm.nih.gov/CCDS/CcdsBrowse.cgi?REQUEST=CCDS&GO=MainBrowse&DATA=', accession),
			uniprot     = if (content.type %in% c(RBIODB.ANY, RBIODB.XML)) paste0('http://www.uniprot.org/uniprot/', accession, '.xml'),
			NULL
			)

		return(url)
	}
}
