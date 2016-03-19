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
	RBIODB.MASSBANK     <- 'massbank'

	# Fields
	RBIODB.COMPOUND     <- 'compound'
	RBIODB.ACCESSION    <- 'accession'
	RBIODB.DESCRIPTION  <- 'description'
	RBIODB.NAME         <- 'name'
	RBIODB.CHEBI.ID     <- 'chebiid'
	RBIODB.LIPIDMAPS.ID <- 'lipidmapsid'
	RBIODB.KEGG.ID      <- 'keggid'
	RBIODB.HMDB.ID      <- 'hmdbid'
	RBIODB.PUBCHEM.ID   <- 'pubchemid'
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

	RBIODB.MSMODE.NEG <- 'neg'
	RBIODB.MSMODE.POS <- 'pos'

	RBIODB.FIELDS <- data.frame(matrix(c(
		# FIELD NAME            CLASS
		RBIODB.COMPOUND,        'BiodEntry',
		RBIODB.ACCESSION,       'character',
		RBIODB.DESCRIPTION,     'character',
		RBIODB.NAME,            'character',
		RBIODB.CHEBI.ID,        'character',
		RBIODB.LIPIDMAPS.ID,    'character',
		RBIODB.KEGG.ID,         'character',
		RBIODB.HMDB.ID,         'character',
		RBIODB.PUBCHEM.ID,      'character',
		RBIODB.INCHI,           'character',
		RBIODB.INCHIKEY,        'character',
		RBIODB.MSDEV,           'character',
		RBIODB.MSDEVTYPE,       'character',
		RBIODB.MSTYPE,          'character',
		RBIODB.MSMODE,          'character',
		RBIODB.MSPRECMZ,        'double',
		RBIODB.MSPRECANNOT,     'character',
		RBIODB.FORMULA,         'character',
		RBIODB.SUPER.CLASS,     'character',
		RBIODB.MASS,            'double',
		RBIODB.AVERAGE.MASS,    'double',
		RBIODB.MONOISOTOPIC.MASS,   'double',
		RBIODB.SEQUENCE,        'character'
		), byrow = TRUE, ncol = 2), stringsAsFactors = FALSE)
	colnames(RBIODB.FIELDS) <- c('name', 'class')

	# How to compute a missing field ?
	RBIODB.FIELD.COMPUTING <- list()
	RBIODB.FIELD.COMPUTING[[RBIODB.INCHI]] <- c(RBIODB.CHEBI)
	RBIODB.FIELD.COMPUTING[[RBIODB.INCHIKEY]] <- c(RBIODB.CHEBI)

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
			NULL
			)

		return(url)
	}
}
