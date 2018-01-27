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
#' @import methods
#' @include biodb-common.R
#' @include Biodb.R
#' @include ChildObject.R
#' @include BiodbEntryField.R
#' @export BiodbEntryFields
#' @exportClass BiodbEntryFields
BiodbEntryFields <- methods::setRefClass("BiodbEntryFields", contains = "ChildObject", fields = list( .fields = "list", .aliasToName = "character" ))

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
		.self$.define(db.info$getEntryIdField(), db.id = TRUE, card = BIODB.CARD.MANY, description = paste(db.info$getName(), 'ID'))
	.self$.define('compound.id', alias = 'compoundid', description = 'The compound ID.')
	.self$.define('cas.id',             description = '', alias = 'casid')

	.self$.define('description',    description = 'The decription of the entry.')
	.self$.define('protdesc',   description = 'Protein description.')

	.self$.define('name',           description = 'The name of the entry.')
	.self$.define('comp.iupac.name.allowed',    description = 'IUPAC allowed name')
	.self$.define('comp.iupac.name.trad',       description = 'IUPAC traditional name')
	.self$.define('comp.iupac.name.syst',       description = 'IUPAC systematic name')
	.self$.define('comp.iupac.name.pref',       description = 'IUPAC preferred name')
	.self$.define('comp.iupac.name.cas',        description = 'IUPAC CAS name')
	.self$.define('fullnames',  description = 'List of names.',     card = BIODB.CARD.MANY)
	.self$.define('synonyms',   description = 'List of synonyms.',  card = BIODB.CARD.MANY)
	.self$.define('symbol',     description = 'A symbol (short name) used to name the entry.')
	.self$.define('gene.symbols',  alias = 'genesymbols', description = 'A list of gene symbols.', card = BIODB.CARD.MANY)

	.self$.define('logp',       description = 'logP',               class = 'double')
	.self$.define('nb.compounds',  alias = 'nbcompounds', description = 'Number of associated compounds.', class = 'integer')
	.self$.define('compounds',     class = 'object',        description = 'List of associated compounds.', card = BIODB.CARD.MANY)

	.self$.define('formula',            description = 'Empirical molecular formula.')
	.self$.define('inchi',      description = 'International Chemical Identifier (InChI).')
	.self$.define('inchikey',   description = 'Hash key of the International Chemical Identifier (InChIKey).')
	.self$.define('smiles',             description = 'SMILES.')
	.self$.define('smiles.canonical',   description = 'SMILES canonical.')
	.self$.define('smiles.isomeric',    description = 'SMILES isomeric.')
	.self$.define('catalytic.activity', description = 'Catalytic activity.',                        card = BIODB.CARD.MANY)
	.self$.define('cofactor',           description = 'Cofactor.',                                  card = BIODB.CARD.MANY)
	.self$.define('charge',             description = 'Charge.',               class = 'integer')

	.self$.define('average.mass',  description = 'Average mass.',    class = 'double')
	.self$.define('monoisotopic.mass',  alias = c('exact.mass'), description = 'Monoisotopic mass.',    class = 'double')
	.self$.define('nominal.mass',       description = 'Nominal mass.',         class = 'integer')
	.self$.define('molecular.mass',     alias = c('mass', 'molecular.weight'), description = 'Molecular mass (also called molecular weight), in Dalton.',     class = 'double')

	.self$.define('super.class',        description = 'Super class.', alias = 'superclass')
	.self$.define('sequence',           description = 'Gene or protein sequence.')
	.self$.define('length',             description = 'Sequence length.',               class = 'integer')
	.self$.define('location',           description = 'Sequence location.')

	.self$.define('msdev',              description = 'Mass spectrometer device.')
	.self$.define('ms.level',           description = 'Mass spectrum level.', class = 'integer')
	.self$.define('msdevtype',          description = 'Mass spectrometer device type.')
	.self$.define('mstype',             description = 'Mass spectrometry type.')
	.self$.define('ms.mode',            description = 'Mass spectrometry mode.', alias = 'msmode')
	.self$.define('msprecmz',           description = 'MS precursor M/Z value.',  class =  'double',    card = BIODB.CARD.MANY)
	.self$.define('msprecannot',        description = 'MS precursor annotation.')
	.self$.define('nb.peaks',           description = 'Number of MS peaks.', alias = 'nbpeaks',            class = 'integer')

	# MS Peaks
	.self$.define('peaks',              description = 'Peaks table.',                class = 'data.frame')
	.self$.define('peak.mz',            description = 'Peak M/Z.',      class = 'double')
	.self$.define('peak.mztheo',        description = 'Peak theoretical M/Z.',  class = 'double')
	.self$.define('peak.mzexp',         description = 'Peak experimental M/Z.',   class = 'double')

	# Chromatographic column
	.self$.define('chrom.col',                  description = 'Chromatographic column.', alias = 'compoundid')
	.self$.define('chrom.col.name',             description = 'Chromatographic column name.')
	.self$.define('chrom.col.method.protocol',  description = 'Chromatographic method protocol.')
	.self$.define('chrom.col.id',               description = 'Chromatographic column ID.')
	.self$.define('chrom.col.constructor',      description = 'Chromatographic column constructor.')
	.self$.define('chrom.col.length',           description = 'Chromatographic column length.', class = "double")
	.self$.define('chrom.col.diameter',         description = 'Chromatographic column diameter.', class = "double")
	.self$.define('chrom.col.rt',               description = 'Chromatographic column retention time.', alias = 'chromcolrt', class = 'double')
	.self$.define('chrom.col.rt.min',           description = 'Chromatographic column retention time minimum.', class = "double")
	.self$.define('chrom.col.rt.max',           description = 'Chromatographic column retention time maximum.', class = "double")
})
