# vi: fdm=marker

# Constants {{{1
################################################################

.BIODB.PUBCHEM.COMP.PARSING.EXPR <- list(
	'accession'                 = "//PC-CompoundType_id_cid",
	'inchi'                     = "//PC-Urn_label[text()='InChI']/../../..//PC-InfoData_value_sval",
	'inchikey'                  = "//PC-Urn_label[text()='InChIKey']/../../..//PC-InfoData_value_sval",
	'formula'                   = "//PC-Urn_label[text()='Molecular Formula']/../../..//PC-InfoData_value_sval",
	'exact.mass'                = "//PC-Urn_label[text()='Mass']/../../..//PC-InfoData_value_fval",
	'molecular.weight'          = "//PC-Urn_label[text()='Molecular Weight']/../../..//PC-InfoData_value_fval",
	'comp.iupac.name.syst'      = "//PC-Urn_label[text()='IUPAC Name']/../PC-Urn_name[text()='Systematic']/../../..//PC-InfoData_value_sval",
	'comp.iupac.name.allowed'   = "//PC-Urn_label[text()='IUPAC Name']/../PC-Urn_name[text()='Allowed']/../../..//PC-InfoData_value_sval",
	'comp.iupac.name.cas'       = "//PC-Urn_label[text()='IUPAC Name']/../PC-Urn_name[text()='CAS-like Style']/../../..//PC-InfoData_value_sval",
	'comp.iupac.name.pref'      = "//PC-Urn_label[text()='IUPAC Name']/../PC-Urn_name[text()='Preferred']/../../..//PC-InfoData_value_sval",
	'comp.iupac.name.trad'      = "//PC-Urn_label[text()='IUPAC Name']/../PC-Urn_name[text()='Traditional']/../../..//PC-InfoData_value_sval",
	'logp'                      = "//PC-Urn_label[text()='Log P']/../../..//PC-InfoData_value_fval",
	'smiles.canonical'          = "//PC-Urn_label[text()='SMILES']/../PC-Urn_name[text()='Canonical']/../../..//PC-InfoData_value_sval",
	'smiles.isomeric'           = "//PC-Urn_label[text()='SMILES']/../PC-Urn_name[text()='Isomeric']/../../..//PC-InfoData_value_sval"
)

# Class declaration {{{1
################################################################

#' @include NcbiPubchemConn.R
#' @include CompounddbConn.R
NcbiPubchemCompConn <- methods::setRefClass("NcbiPubchemCompConn", contains = c("NcbiPubchemConn", 'CompounddbConn'))

# Constructor {{{1
################################################################

NcbiPubchemCompConn$methods( initialize = function(...) {
	callSuper(db.name = 'compound', id.xmltag = 'PC-CompoundType_id_cid', entry.xmltag = 'PC-Compound', id.urlfield = 'cid', entrez.name = 'pccompound', ...)
})

# Search compound {{{1
################################################################

NcbiPubchemCompConn$methods( searchCompound = function(name = NULL, mass = NULL, mass.field = NULL, mass.tol = 0.01, mass.tol.unit = 'plain', max.results = NA_integer_) {
		
	.self$.checkMassField(mass = mass, mass.field = mass.field)

	term <- character()

	# Search by name
	if ( ! is.null(name))
		term <- paste0('"', name, '"', '[IUPACName]')

	# Search by mass
	if ( ! is.null(mass) && ! is.null(mass.field)) {

		mass.field <- .self$getBiodb()$getEntryFields()$getRealName(mass.field)

		if ( ! mass.field %in% c('monoisotopic.mass' ,'molecular.mass'))
			.self$message('caution', paste0('Mass field "', mass.field, '" is not handled.'))

		else {

			pubchem.mass.field <- if (mass.field == 'monoisotopic.mass') 'MonoisotopicMass' else 'MolecularWeight'

			if (mass.tol.unit == 'ppm') {
				mass.min <- mass * (1 - mass.tol * 1e-6)
				mass.max <- mass * (1 + mass.tol * 1e-6)
			} else {
				mass.min <- mass - mass.tol
				mass.max <- mass + mass.tol
			}

			mass.term <- paste0(mass.min, ':', mass.max, '[', pubchem.mass.field, ']')

			if (length(term) > 0)
				term <- paste(term, 'AND', mass.term)
			else
				term <- mass.term
		}
	}

	# Set retmax
	if (is.na(max.results)) {
		xml <- .self$ws.esearch(term = term, retmax = 0, retfmt = 'parsed')
		retmax <- as.integer(XML::xpathSApply(xml, "/eSearchResult/Count", XML::xmlValue))
		if (length(retmax) == 0)
			retmax = NA_integer_
	}
	else
		retmax <- max.results

	# Send request
	ids <- .self$ws.esearch(term = term, retmax = retmax, retfmt = 'ids')

	return(ids)
})

# Private methods {{{1
################################################################

# Get parsing expressions {{{2
################################################################

NcbiPubchemCompConn$methods( .getParsingExpressions = function() {
	return(.BIODB.PUBCHEM.COMP.PARSING.EXPR)
})
