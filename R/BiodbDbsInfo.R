# vi: fdm=marker

# Class declaration {{{1
################################################################

#' A class for describing the available databases.
#'
#' The unique instance of this class is handle by the \code{\link{Biodb}} class and accessed through the \code{getDbsInfo()} method.
#'
#' @seealso \code{\link{Biodb}}, \code{\link{BiodbDbInfo}}.
#'
#' @param db.id A database ID, as a character string.
#'
#' @examples
#' # Getting the base URL of a database:
#' mybiodb <- biodb::Biodb()
#' chebi.base.url <- mybiodb$getDbsInfo()$get('chebi')$getBaseUrl()
#'
#' # Setting a token:
#' mybiodb$getDbsInfo()$get('chemspider')$setToken('my.chemspider.token')
#'
#' @import methods
#' @include ChildObject.R
#' @include BiodbDbInfo.R
#' @export BiodbDbsInfo
#' @exportClass BiodbDbsInfo
BiodbDbsInfo <- methods::setRefClass("BiodbDbsInfo", contains =  "ChildObject", fields = list( .dbs = "list"))

# Constructor {{{1
################################################################

BiodbDbsInfo$methods( initialize = function(...) {

	callSuper(...)

	.dbs <<- list()

	.self$.initDbsInfo()
})

# Get list of database IDs {{{1
################################################################

BiodbDbsInfo$methods( getIds = function() {
	":\n\nReturns a character vector containing all the IDs of the defined databases."

	return(names(.self$.dbs))
})

# Is defined {{{1
################################################################

BiodbDbsInfo$methods( isDefined = function(db.id) {
	":\n\nReturns TRUE if the specified id corresponds to a defined database."

	return(db.id %in% names(.self$.dbs))
})

# Check is defined {{{1
################################################################

BiodbDbsInfo$methods( checkIsDefined = function(db.id) {
	":\n\nThrows an error if the specified id does not correspond to a defined database."

	if ( ! .self$isDefined(db.id))
		.self$message('error', paste("Database \"", db.id, "\" is not defined.", sep = ''))
})

# Get {{{1
################################################################

BiodbDbsInfo$methods( get = function(db.id) {
	":\n\nReturns the BiodbDbInfo instance corresponding to the specified database ID."

	.self$checkIsDefined(db.id)
	db <- .self$.dbs[[db.id]]
	return(db)
})

# Get all {{{1
################################################################

BiodbDbsInfo$methods( getAll = function() {
	":\n\nReturns a list of all BiodbDbInfo instances."

	return(unname(.self$.dbs))
})

# Show {{{1
################################################################

BiodbDbsInfo$methods( show = function() {
	cat("Biodb databases information instance.\n")
})

# Private methods {{{1
################################################################

# Initialize databases information {{{2
################################################################

BiodbDbsInfo$methods( .initDbsInfo = function() {
	.self$.define('chebi',                  name = 'ChEBI',         scheduler.n = 3, entry.content.type = 'xml', base.url = 'https://www.ebi.ac.uk/chebi/', ws.url = 'https://www.ebi.ac.uk/webservices/chebi/2.0/test/', xml.ns = "https://www.ebi.ac.uk/webservices/chebi")
	.self$.define('chemspider',             name = 'ChemSpider',    scheduler.n = 3, entry.content.type = 'xml', base.url = "http://www.chemspider.com/", xml.ns = "http://www.chemspider.com/")
	.self$.define('expasy.enzyme',          name = 'ExPASy ENZYME', scheduler.n = 3, entry.content.type = 'txt', base.url = "https://enzyme.expasy.org/")
	.self$.define('hmdb.metabolites',       name = 'HMDB Metabolites', scheduler.n = 3, entry.content.type = 'xml', base.url = "http://www.hmdb.ca/",       xml.ns = 'http://www.hmdb.ca')
	.self$.define('kegg.compound',          name = 'KEGG Compound', scheduler.n = 3, entry.content.type = 'txt', base.url = 'http://www.kegg.jp/', ws.url = 'http://rest.kegg.jp/')
	.self$.define('lipidmaps.structure',    name = 'LIPID MAPS Structure', scheduler.n = 1, scheduler.t = 20, entry.content.type = 'csv', base.url = 'http://www.lipidmaps.org/data/') # About access frequency, see http://www.lipidmaps.org/data/structure/programmaticaccess.html
	.self$.define('mass.csv.file',          name = 'Mass CSV File',                  entry.content.type = 'tsv')
#	.self$.define('massbank.eu') # Turn Off name = '', Massbank Europe, does not work very well when searching for spectra through API.
	.self$.define('massbank.jp',            name = 'MassBank Japan', scheduler.n = 3, entry.content.type = 'txt', base.url = 'http://www.massbank.jp/')
	.self$.define('mirbase.mature',         name = 'miRBase Mature', scheduler.n = 3, entry.content.type = 'txt', base.url = "http://www.mirbase.org/")
	.self$.define('ncbi.ccds',              name = 'NCBI CCDS', scheduler.n = 3, entry.content.type = 'html', base.url = 'https://www.ncbi.nlm.nih.gov/CCDS/')
	.self$.define('ncbi.gene',              name = 'NCBI Gene', scheduler.n = 3, entry.content.type = 'xml', base.url = 'https://www.ncbi.nlm.nih.gov/', ws.url = 'https://eutils.ncbi.nlm.nih.gov/entrez/eutils/') # About access frequency, see https://www.ncbi.nlm.nih.gov/books/NBK25497/
	.self$.define('ncbi.pubchem.comp',      name = 'PubChem Compound', scheduler.n = 5, entry.content.type = 'xml', base.url = 'https://pubchem.ncbi.nlm.nih.gov/', ws.url = 'https://pubchem.ncbi.nlm.nih.gov/rest/pug/') # About access frequency, see https://pubchem.ncbi.nlm.nih.gov/pug_rest/PUG_REST.html
	.self$.define('ncbi.pubchem.subst',     name = 'PubChem Substance', scheduler.n = 5, entry.content.type = 'xml', base.url = 'https://pubchem.ncbi.nlm.nih.gov/', ws.url = 'https://pubchem.ncbi.nlm.nih.gov/rest/pug/') # About access frequency, see https://pubchem.ncbi.nlm.nih.gov/pug_rest/PUG_REST.html
	.self$.define('peakforest.mass',        name = 'PeakForest Mass', scheduler.n = 3, entry.content.type = 'json', base.url = 'https://rest.peakforest.org/')
	.self$.define('peakforest.compound',    name = 'PeakForest Compound', scheduler.n = 3, entry.content.type = 'json', base.url = 'https://rest.peakforest.org/')
	.self$.define('uniprot',                name = 'UniProt',                  entry.content.type = 'xml', base.url = 'http://www.uniprot.org/uniprot/', xml.ns = "http://uniprot.org/uniprot")
})

# Define {{{2
################################################################

BiodbDbsInfo$methods( .define = function(id, ...) {

	# Is this database already defined?
	if (id %in% names(.self$.dbs))
		.self$message('error', paste("Database \"", id, "\" has already been defined.", sep = ''))

	# Define new field
	.self$.dbs[[id]] <- BiodbDbInfo$new(parent = .self, id = id, ...)
})
