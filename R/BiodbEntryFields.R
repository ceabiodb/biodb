# vi: fdm=marker

# Class declaration {{{1
################################################################

#' A class for handling description of all entry fields.
#'
#' The unique instance of this class is handle by the \code{\link{Biodb}} class and accessed through the \code{getEntryFields()} method.
#'
#' @seealso \code{\link{Biodb}}, \code{\link{BiodbEntryField}}.
#'
#' @param name      The name or alias of a field.
#' @param database  The name of a database.
#'
#' @examples
#' # Getting information about the accession field:
#' mybiodb <- biodb::Biodb()
#' entry.field <- mybiodb$getEntryFields()$get('accession')
#'
#' # Test if a name is an alias of a field
#' mybiodb$getEntryFields()$isAlias('genesymbols')
#'
#' # Test if a name is associated with a defined field
#' mybiodb$getEntryFields()$isDefined('chebi.id')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @import methods
#' @include Biodb.R
#' @include BiodbChildObject.R
#' @include BiodbEntryField.R
#' @export BiodbEntryFields
#' @exportClass BiodbEntryFields
BiodbEntryFields <- methods::setRefClass("BiodbEntryFields", contains = "BiodbChildObject", fields = list( .fields = "list", .aliasToName = "character" ))

# Constructor {{{1
################################################################

BiodbEntryFields$methods( initialize = function(...) {

	callSuper(...)

	.fields <<- list()
	.aliasToName <<- character(0)

	.self$.initFields()
})

# Is alias {{{1
################################################################

BiodbEntryFields$methods( isAlias = function(name) {
	":\n\nReturns TRUE if name is an alias of a field."

	return(tolower(name) %in% names(.self$.aliasToName))
})

# Is defined {{{1
################################################################

BiodbEntryFields$methods( isDefined = function(name) {
	":\n\nReturns TRUE if name corresponds to a defined field."
	return(tolower(name) %in% names(.self$.fields) || .self$isAlias(name))
})

# Check is defined {{{1
################################################################

BiodbEntryFields$methods( checkIsDefined = function(name) {
	":\n\nThrows an error if name does not correspond to a defined field."

	if ( ! .self$isDefined(name))
		.self$message('error', paste("Field \"", name, "\" is not defined.", sep = ''))
})

# Get real name {{{1
################################################################

BiodbEntryFields$methods( getRealName = function(name) {
	":\n\nIf name is an alias, returns the main name of the field. If name is not found neither in aliases nor in real names, an error is thrown."

	.self$checkIsDefined(name)

	if ( ! tolower(name) %in% names(.self$.fields))
		name <- .self$.aliasToName[[tolower(name)]]

	return(name)
})

# Get {{{1
################################################################

BiodbEntryFields$methods( get = function(name) {
	":\n\nReturns the BiodbEntryField instance associated with name."

	name <- .self$getRealName(name)
	field <- .self$.fields[[tolower(name)]]
	return(field)
})

# Get field names {{{1
################################################################

BiodbEntryFields$methods( getFieldNames = function(type = NULL) {
	":\n\nReturns the main names of all fields."

	# Filter by type
	if ( ! is.null(type)) {
		fields <- character()
		for (n in names(.self$.fields))
			if (.self$.fields[[n]]$getType() %in% type)
				fields <- c(fields, n)
	}

	else
		fields <- names(.self$.fields)

	return(sort(fields))
})

# Get database id field {{{1
################################################################

BiodbEntryFields$methods( getDatabaseIdField = function(database) {
	":\n\nReturns the name of the field handling identifiers (i.e.: accession numbers) for this database."

	return(.self$get(.self$getBiodb()$getDbsInfo()$get(database)$getIdFieldName()))
})

# Show {{{1
################################################################

BiodbEntryFields$methods( show = function() {
	cat("Biodb entry fields information instance.\n")
})

# Private methods {{{1
################################################################

# Define {{{2
################################################################

BiodbEntryFields$methods( .define = function(name, ...) {

	# Check that name is in lower case
	if (name != tolower(name))
		.self$message('error', paste("Field name \"", name, "\" must be in lower case.", sep = ''))

	# Is field already defined?
	if (.self$isDefined(name))
		.self$message('error', paste("Field \"", name, "\" has already been defined.", sep = ''))

	# Define new field
	field <- BiodbEntryField$new(parent = .self, name = name, ...)

	# Store inside fields list
	.self$.fields[[name]] <- field

	# Define aliases
	if (field$hasAliases())
		for (alias in field$getAliases())
			.self$.aliasToName[[alias]] <- name
})

# Init fields {{{2
################################################################

BiodbEntryFields$methods( .initFields = function() {

	.self$.define('accession',      description = 'The accession number of the entry.')
	# Define database ID fields
	for (db.info in .self$getBiodb()$getDbsInfo()$getAll())
		.self$.define(db.info$getEntryIdField(), db.id = TRUE, card = BIODB.CARD.MANY, description = paste(db.info$getName(), 'ID'), forbids.duplicates = TRUE, case.insensitive = TRUE, type = 'id')
	.self$.define('compound.id',        description = 'The compound ID.', card = BIODB.CARD.MANY, forbids.duplicates = TRUE, case.insensitive = TRUE, type = 'id', alias = 'compoundid')
	.self$.define('cas.id',             description = 'CAS ID',           card = BIODB.CARD.MANY, forbids.duplicates = TRUE, case.insensitive = TRUE, type = 'id', alias = 'casid')
	.self$.define('kegg.genes.id',      description = 'KEGG Genes ID',           card = BIODB.CARD.MANY, forbids.duplicates = TRUE, case.insensitive = TRUE, type = 'id')

	.self$.define('description',    description = 'The decription of the entry.', alias = 'protdesc')

	.self$.define('name',       description = 'The name of the entry.',     card = BIODB.CARD.MANY, alias = c('fullnames', 'synonyms'), case.insensitive = TRUE, forbids.duplicates = TRUE, type = 'name')
	.self$.define('comp.iupac.name.allowed',    description = 'IUPAC allowed name', type = 'name')
	.self$.define('comp.iupac.name.trad',       description = 'IUPAC traditional name', type = 'name')
	.self$.define('comp.iupac.name.syst',       description = 'IUPAC systematic name', type = 'name')
	.self$.define('comp.iupac.name.pref',       description = 'IUPAC preferred name', type = 'name')
	.self$.define('comp.iupac.name.cas',        description = 'IUPAC CAS name', type = 'name')
	.self$.define('gene.symbol',  alias = c('gene.symbols', 'symbol', 'genesymbols'), description = 'A list of gene symbols.', card = BIODB.CARD.MANY, case.insensitive = TRUE, forbids.duplicates = TRUE)

	.self$.define('logp',       description = 'logP',               class = 'double')
	.self$.define('nb.compounds',  alias = 'nbcompounds', description = 'Number of associated compounds.', class = 'integer')
#	.self$.define('compounds',     class = 'object',        description = 'List of associated compounds.', card = BIODB.CARD.MANY)

	.self$.define('formula',            description = 'Empirical molecular formula.')
	.self$.define('inchi',      description = 'International Chemical Identifier (InChI).', computable.from = 'chebi')
	.self$.define('inchikey',   description = 'Hash key of the International Chemical Identifier (InChIKey).', computable.from = 'chebi')
	.self$.define('smiles',             description = 'SMILES.')
	.self$.define('smiles.canonical',   description = 'SMILES canonical.')
	.self$.define('smiles.isomeric',    description = 'SMILES isomeric.')
	.self$.define('catalytic.activity', description = 'Catalytic activity.',                        card = BIODB.CARD.MANY)
	.self$.define('cofactor',           description = 'Cofactor.',                                  card = BIODB.CARD.MANY)
	.self$.define('charge',             description = 'Charge.',               class = 'integer')

	.self$.define('average.mass',  description = 'Average mass.',    class = 'double', type = 'mass')
	.self$.define('monoisotopic.mass',  alias = c('exact.mass'), description = 'Monoisotopic mass.',    class = 'double', type = 'mass')
	.self$.define('nominal.mass',       description = 'Nominal mass.',         class = 'integer', type = 'mass')
	.self$.define('molecular.mass',     alias = c('mass', 'molecular.weight'), description = 'Molecular mass (also called molecular weight), in Dalton.',     class = 'double', type = 'mass', computable.from = 'chebi')

	.self$.define('comp.super.class',        description = 'Compound super class.', alias = c('superclass', 'super.class'))
	.self$.define('sequence',           description = 'Gene or protein sequence.', computable.from = 'ncbi.ccds')
	.self$.define('seq.length',             description = 'Sequence length.',               class = 'integer', alias = 'length')
	.self$.define('seq.location',           description = 'Sequence location.', alias = 'location')

	.self$.define('msdev',              description = 'Mass spectrometer device.')
	.self$.define('ms.level',           description = 'Mass spectrum level.', class = 'integer')
	.self$.define('msdevtype',          description = 'Mass spectrometer device type.')
	.self$.define('mstype',             description = 'Mass spectrometry type.')
	.self$.define('ms.mode',            description = 'Mass spectrometry mode.', alias = 'msmode', allowed.values = list(neg = c('-', 'negative'), pos = c('+', 'positive')), lower.case = TRUE)
	.self$.define('msprecmz',           description = 'MS precursor M/Z value.',  class =  'double',    card = BIODB.CARD.MANY)
	.self$.define('msprecannot',        description = 'MS precursor annotation.')
	.self$.define('nb.peaks',           description = 'Number of MS peaks.', alias = 'nbpeaks',            class = 'integer')

	# MS Peaks
	.self$.define('peaks',              description = 'Peaks table.',                class = 'data.frame')
	.self$.define('peak.mz',            description = 'Peak M/Z.',      class = 'double', group = 'peak')
	.self$.define('peak.mztheo',        description = 'Peak theoretical M/Z.',  class = 'double', group = 'peak')
	.self$.define('peak.mzexp',         description = 'Peak experimental M/Z.',   class = 'double', group = 'peak')
	.self$.define('peak.attr',          description = 'Peak attribution.', group = 'peak')
	.self$.define('peak.relative.intensity',          description = 'Peak relative intensity, in percentage (from 0 to 100).', class = 'double', group = 'peak')
	.self$.define('peak.intensity',          description = 'Peak intensity.', class = 'double', group = 'peak')
	.self$.define('peak.formula',            description = 'Peak formula.', group = 'peak')
	.self$.define('peak.comp',            description = 'Peak comp.', group = 'peak')
	.self$.define('peak.mass',            description = 'Peak mass.', class = 'double', group = 'peak')
	.self$.define('peak.error.ppm',            description = 'Peak error in PPM.', class = 'double', group = 'peak')

	# Chromatographic column
	.self$.define('chrom.col.name',             description = 'Chromatographic column name.', alias = c('chrom.col', 'chromcol'))
	.self$.define('chrom.col.method.protocol',  description = 'Chromatographic method protocol.')
	.self$.define('chrom.col.id',               description = 'Chromatographic column ID.')
	.self$.define('chrom.col.constructor',      description = 'Chromatographic column constructor.')
	.self$.define('chrom.col.length',           description = 'Chromatographic column length.', class = "double")
	.self$.define('chrom.col.diameter',         description = 'Chromatographic column diameter.', class = "double")
	.self$.define('chrom.solvent',         description = 'Chromatographic column solvent.', card = BIODB.CARD.MANY)
	.self$.define('chrom.flow.rate',         description = 'Chromatographic column flow rate.')
	.self$.define('chrom.flow.gradient',         description = 'Chromatographic column flow gradient.')
	.self$.define('chrom.rt',               description = 'Chromatographic column retention time.', alias = c('chromcolrt', 'chrom.col.rt'), class = 'double')
	.self$.define('chrom.rt.min',           description = 'Chromatographic column retention time minimum.', class = "double", alias = 'chrom.col.rt.min')
	.self$.define('chrom.rt.max',           description = 'Chromatographic column retention time maximum.', class = "double", alias = 'chrom.col.rt.max')
	.self$.define('chrom.rt.unit',          description = 'Chromatographic column retention time unit.', alias = 'chrom.col.rt.unit', allowed.values = c('min', 's'), lower.case = TRUE)
})
