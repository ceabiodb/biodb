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

}
