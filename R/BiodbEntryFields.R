# vi: fdm=marker

# Class declaration {{{1
################################################################

#' A class for handling description of all entry fields.
#'
#' The unique instance of this class is handle by the \code{\link{Biodb}} class and accessed through the \code{getEntryFields()} method.
#'
#' @seealso \code{\link{Biodb}}, \code{\link{BiodbEntryField}}.
#'
#' @examples
#' # Getting information about the accession field:
#' mybiodb <- biodb::Biodb()
#' entry.field <- mybiodb$getEntryFields()$get('accession')
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

# Init fields {{{1
################################################################

BiodbEntryFields$methods( .initFields = function() {
	.self$.define('accession',  description = 'Accession number of an entry.')
	.self$.define('description')
	.self$.define('name')
	.self$.define('comp.iupac.name.allowed')
	.self$.define('comp.iupac.name.trad')
	.self$.define('comp.iupac.name.syst')
	.self$.define('comp.iupac.name.pref')
	.self$.define('comp.iupac.name.cas')
	.self$.define('logp',       description = 'logP', class = 'double')
	.self$.define('fullnames',                         card = BIODB.CARD.MANY)
	.self$.define('synonyms',                          card = BIODB.CARD.MANY)
	.self$.define('protdesc',   description = 'Protein description.')
	.self$.define('symbol')
	.self$.define('gene.symbols',  alias = 'genesymbols',                    card = BIODB.CARD.MANY)
	.self$.define('nb.compounds',  alias = 'nbcompounds', class = 'integer')
	.self$.define('compounds',     class = 'object',   card = BIODB.CARD.MANY)
	.self$.define('compound.id', alias = 'compoundid')

	# Define database ID fields
	for (db.info in .self$getBiodb()$getDbsInfo()$getAll())
		.self$.define(db.info$getEntryIdField(), db.id = TRUE, card = BIODB.CARD.MANY)

	.self$.define('inchi')
	.self$.define('inchikey')

	.self$.define('msdev')
	.self$.define('ms.level', class = 'integer')
	.self$.define('msdevtype')
	.self$.define('mstype')
	.self$.define('ms.mode', alias = 'msmode')
	.self$.define('msprecmz',     class =  'double',    card = BIODB.CARD.MANY)
	.self$.define('msprecannot')
	.self$.define('formula')
	.self$.define('super.class', alias = 'superclass')
	.self$.define('mass',         class = 'double')
	.self$.define('average.mass', class = 'double')
	.self$.define('monoisotopic.mass',    class = 'double')
	.self$.define('nominal.mass',         class = 'integer')
	.self$.define('molecular.weight',     class = 'double')
	.self$.define('sequence')
	.self$.define('length',               class = 'integer')
	.self$.define('location')
	.self$.define('nb.peaks', alias = 'nbpeaks',            class = 'integer')
	.self$.define('smiles')
	.self$.define('smiles.canonical')
	.self$.define('smiles.isomeric')
	.self$.define('catalytic.activity',                        card = BIODB.CARD.MANY)
	.self$.define('cofactor',                                  card = BIODB.CARD.MANY)
	.self$.define('charge',               class = 'integer')
	.self$.define('cas.id', alias = 'casid')

	# MS Peaks
	.self$.define('peaks',                class = 'data.frame')
	.self$.define('peak.mz',      class = 'double')
	.self$.define('peak.mztheo',  class = 'double')
	.self$.define('peak.mzexp',   class = 'double')

	# Chromatographic column
	.self$.define('chrom.col', alias = 'compoundid')
	.self$.define('chrom.col.name')
	.self$.define('chrom.col.method.protocol')
	.self$.define('chrom.col.id')
	.self$.define('chrom.col.constructor')
	.self$.define('chrom.col.length', class = "double")
	.self$.define('chrom.col.diameter', class = "double")
	.self$.define('chrom.col.rt', alias = 'chromcolrt', class = 'double')
	.self$.define('chrom.col.rt.min', class = "double")
	.self$.define('chrom.col.rt.max', class = "double")
})

# Is alias {{{1
################################################################

BiodbEntryFields$methods( isAlias = function(name) {
	return(tolower(name) %in% names(.self$.aliasToName))
})

# Is defined {{{1
################################################################

BiodbEntryFields$methods( isDefined = function(name) {
	return(tolower(name) %in% names(.self$.fields) || .self$isAlias(name))
})

# Check is defined {{{1
################################################################

BiodbEntryFields$methods( checkIsDefined = function(name) {
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
	name <- .self$getRealName(name)
	field <- .self$.fields[[tolower(name)]]
	return(field)
})

# Get database id field {{{1
################################################################

BiodbEntryFields$methods( getDatabaseIdField = function(database) {
	return(.self$get(.self$getBiodb()$getDbsInfo()$get(database)$getIdFieldName()))
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
