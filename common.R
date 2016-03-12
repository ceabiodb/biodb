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

	# Class names
	RBIODB.CHEBI <- 'chebi'
	RBIODB.KEGG  <- 'kegg'
	RBIODB.PUBCHEM  <- 'pubchem'
	RBIODB.MASSBANK  <- 'massbank'

	# Fields
	RBIODB.COMPOUND     <- 'compound'
	RBIODB.ACCESSION    <- 'accession'
	RBIODB.NAME         <- 'name'
	RBIODB.CHEBI.ID     <- 'chebiid'
	RBIODB.LIPIDMAPS.ID <- 'lipidmapsid'
	RBIODB.KEGG.ID      <- 'keggid'
	RBIODB.PUBCHEM.ID   <- 'pubchemid'
	RBIODB.INCHI        <- 'inchi'
	RBIODB.INCHIKEY     <- 'inchikey'
	RBIODB.MSDEV        <- 'msdev'
	RBIODB.MSDEVTYPE    <- 'msdevtype'
	RBIODB.MSTYPE       <- 'mstype'
	RBIODB.MSMODE       <- 'msmode'
	RBIODB.MSPRECMZ     <- 'msprecmz'       # numeric
	RBIODB.MSPRECANNOT  <- 'msprecannot'

	RBIODB.MSMODE.NEG <- 'neg'
	RBIODB.MSMODE.POS <- 'pos'

	RBIODB.FIELDS <- data.frame(matrix(c(
		# FIELD NAME            CLASS
		RBIODB.COMPOUND,        'BiodEntry',
		RBIODB.ACCESSION,       'character',
		RBIODB.NAME,            'character',
		RBIODB.CHEBI.ID,        'character',
		RBIODB.LIPIDMAPS.ID,    'character',
		RBIODB.KEGG.ID,         'character',
		RBIODB.PUBCHEM.ID,      'character',
		RBIODB.INCHI,           'character',
		RBIODB.INCHIKEY,        'character',
		RBIODB.MSDEV,           'character',
		RBIODB.MSDEVTYPE,       'character',
		RBIODB.MSTYPE,          'character',
		RBIODB.MSMODE,          'character',
		RBIODB.MSPRECMZ,        'double',
		RBIODB.MSPRECANNOT,     'character'
		), byrow = TRUE, ncol = 2), stringsAsFactors = FALSE)
	colnames(RBIODB.FIELDS) <- c('name', 'class')

	# How to compute a missing field ?
	RBIODB.FIELD.COMPUTING <- list(
		RBIODB.INCHIKEY = c(RBIODB.CHEBI),
		RBIODB.INCHI    = c(RBIODB.CHEBI)
		)

}
