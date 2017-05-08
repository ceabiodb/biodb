# vi: fdm=marker

# Class declaration {{{1
################################################################

#' A class for handing description of all entry fields.
#'
#' The unique instance of this class is handle by the \code{\link{Biodb}} class and accessed through the \code{getEntryFields()} method.
#'
#' @seealso \code{\link{BiodbEntryField}}.
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
#' @exportClass BiodbEntryFields
BiodbEntryFields <- methods::setRefClass("BiodbEntryFields", contains = "ChildObject", fields = list( .fields = "list" ))

# constructor {{{1
################################################################

BiodbEntryFields$methods( initialize = function(...) {

	callSuper(...)

	.fields <<- list()

	.self$.initFields()
})

# Init fields {{{1
################################################################

BiodbEntryFields$methods( .initFields = function() {
	.self$.define(BIODB.ACCESSION)
	.self$.define(BIODB.DESCRIPTION)
	.self$.define(BIODB.NAME)
	.self$.define(BIODB.COMP.IUPAC.NAME.ALLOWED)
	.self$.define(BIODB.COMP.IUPAC.NAME.TRAD)
	.self$.define(BIODB.COMP.IUPAC.NAME.SYST)
	.self$.define(BIODB.COMP.IUPAC.NAME.PREF)
	.self$.define(BIODB.COMP.IUPAC.NAME.CAS)
	.self$.define(BIODB.COMP.LOGP, class = 'double')
	.self$.define(BIODB.FULLNAMES,                         card = BIODB.CARD.MANY)
	.self$.define(BIODB.SYNONYMS,                          card = BIODB.CARD.MANY)
	.self$.define(BIODB.PROTEIN.DESCRIPTION)
	.self$.define(BIODB.SYMBOL)
	.self$.define(BIODB.GENE.SYMBOLS,                      card = BIODB.CARD.MANY)
	.self$.define(BIODB.NB.COMPOUNDS,  class = 'integer')
	.self$.define(BIODB.COMPOUNDS,     class = 'object',   card = BIODB.CARD.MANY)
	.self$.define(BIODB.COMPOUND.ID)

	.self$.define(BIODB.CHEBI.ID)
	.self$.define(BIODB.CHEMSPIDER.ID)
	.self$.define(BIODB.EXPASY.ENZYME.ID)
	.self$.define(BIODB.HMDB.METABOLITE.ID)
	.self$.define(BIODB.KEGG.COMPOUND.ID)
	.self$.define(BIODB.LIPIDMAPS.STRUCTURE.ID)
	.self$.define(BIODB.NCBI.CCDS.ID)
	.self$.define(BIODB.NCBI.GENE.ID)
	.self$.define(BIODB.NCBI.PUBCHEM.COMP.ID,           card = BIODB.CARD.MANY)
	.self$.define(BIODB.NCBI.PUBCHEM.SUBST.ID)
	.self$.define(BIODB.PEAKFOREST.LCMS.ID)
	.self$.define(BIODB.PEAKFOREST.COMPOUND.ID, card = BIODB.CARD.MANY)
	.self$.define(BIODB.UNIPROT.ID)
	.self$.define(BIODB.INCHI)
	.self$.define(BIODB.INCHIKEY)

	.self$.define(BIODB.MSDEV)
	.self$.define(BIODB.MS.LEVEL, class = 'integer')
	.self$.define(BIODB.MSDEVTYPE)
	.self$.define(BIODB.MSTYPE)
	.self$.define(BIODB.MSMODE)
	.self$.define(BIODB.MSPRECMZ,     class =  'double')
	.self$.define(BIODB.MSPRECANNOT)
	.self$.define(BIODB.FORMULA)
	.self$.define(BIODB.SUPER.CLASS)
	.self$.define(BIODB.MASS,         class = 'double')
	.self$.define(BIODB.AVERAGE.MASS, class = 'double')
	.self$.define(BIODB.MONOISOTOPIC.MASS,    class = 'double')
	.self$.define(BIODB.NOMINAL.MASS,         class = 'integer')
	.self$.define(BIODB.MOLECULAR.WEIGHT,     class = 'double')
	.self$.define(BIODB.SEQUENCE)
	.self$.define(BIODB.LENGTH,               class = 'integer')
	.self$.define(BIODB.LOCATION)
	.self$.define(BIODB.NB.PEAKS,             class = 'integer')
	.self$.define(BIODB.SMILES)
	.self$.define(BIODB.SMILES.CANONICAL)
	.self$.define(BIODB.SMILES.ISOMERIC)
	.self$.define(BIODB.CATALYTIC.ACTIVITY,                        card = BIODB.CARD.MANY)
	.self$.define(BIODB.COFACTOR,                                  card = BIODB.CARD.MANY)
	.self$.define(BIODB.CHARGE,               class = 'integer')
	.self$.define(BIODB.CAS.ID)

	# MS Peaks
	.self$.define(BIODB.PEAKS,                class = 'data.frame')
	.self$.define(BIODB.PEAK.MZ,      class = 'double')
	.self$.define(BIODB.PEAK.MZTHEO,  class = 'double')
	.self$.define(BIODB.PEAK.MZEXP,   class = 'double')

	# Chromatographic column
	.self$.define(BIODB.CHROM.COL)
	.self$.define(BIODB.CHROM.COL.NAME)
	.self$.define(BIODB.CHROM.COL.METHOD.PROTOCOL)
	.self$.define(BIODB.CHROM.COL.ID)
	.self$.define(BIODB.CHROM.COL.CONSTRUCTOR)
	.self$.define(BIODB.CHROM.COL.LENGTH, class = "double")
	.self$.define(BIODB.CHROM.COL.DIAMETER, class = "double")
	.self$.define(BIODB.CHROM.COL.RT, class = 'double')
	.self$.define(BIODB.CHROM.COL.RT.MIN, class = "double")
	.self$.define(BIODB.CHROM.COL.RT.MAX, class = "double")
})

# Define {{{1
################################################################

BiodbEntryFields$methods( .define = function(name, ...) {

	# Is field already defined?
	if (name %in% names(.self$.fields))
		.self$message(MSG.ERROR, paste("Field \"", name, "\" has already been defined.", sep = ''))

	# Define new field
	.self$.fields[[name]] <- BiodbEntryField$new(parent = .self, name = name, ...)
})

# Is defined {{{1
################################################################

BiodbEntryFields$methods( isDefined = function(name) {
	return(name %in% names(.self$.fields))
})

# Check is defined {{{1
################################################################

BiodbEntryFields$methods( checkIsDefined = function(name) {
	if ( ! .self$isDefined(name))
		.self$message(MSG.ERROR, paste("Field \"", name, "\" is not defined.", sep = ''))
})

# Get {{{1
################################################################

BiodbEntryFields$methods( get = function(name) {
	.self$checkIsDefined(name)
	field <- .self$.fields[[name]]
	return(field)
})

# Get database id field {{{1
################################################################

BiodbEntryFields$methods( getDatabaseIdField = function(database) {

	if ( ! database %in% BIODB.DATABASES)
		.self$message(MSG.ERROR, paste("Unknown database \"", database, "\"."))

	return(.self$get(paste(database, 'id', sep = '.')))
})
