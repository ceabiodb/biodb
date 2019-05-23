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
#' chebi.base.url <- mybiodb$getDbsInfo()$get('chebi')$getPropValSlot('urls', 'base.url')
#'
#' # Setting a token:
#' mybiodb$getDbsInfo()$get('chemspider')$setToken('my.chemspider.token')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @import methods
#' @include BiodbChildObject.R
#' @include BiodbDbInfo.R
#' @export BiodbDbsInfo
#' @exportClass BiodbDbsInfo
BiodbDbsInfo <- methods::setRefClass("BiodbDbsInfo", contains =  "BiodbChildObject", fields = list( .dbs = "list"))

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

# Load info file {{{1
################################################################

BiodbDbsInfo$methods( loadInfoFile = function(file) {
	'Load databases information file, and defines the new databases.
	The parameter file must point to a valid JSON file.'

	dbi <- jsonlite::fromJSON(file, simplifyDataFrame = FALSE)

	# Loop on all db info
	for (db in names(dbi))
		.self$.define(db, properties = dbi[[db]])
})

# Private methods {{{1
################################################################

# Initialize databases information {{{2
################################################################

BiodbDbsInfo$methods( .initDbsInfo = function() {

	# Load JSON databases information file
	dbf <- .self$getBiodb()$getConfig()$get('dbinfo.file')
	.self$loadInfoFile(dbf)

#	.self$.define('chebi',                  properties = list(name = 'ChEBI',  scheduler.n = 3, entry.content.type = 'xml', entry.content.encoding = 'UTF-8', urls = c(base.url = 'https://www.ebi.ac.uk/chebi/', ws.url = 'https://www.ebi.ac.uk/webservices/chebi/2.0/'), xml.ns = c(chebi = "https://www.ebi.ac.uk/webservices/chebi", xsd = "http://www.w3.org/2001/XMLSchema")))
	.self$.define('chemspider',             properties = list(name = 'ChemSpider',  scheduler.n = 3, entry.content.type = 'json', urls = c(base.url = "http://www.chemspider.com/", ws.url = "https://api.rsc.org/compounds/v1/")))
	.self$.define('expasy.enzyme',          properties = list(name = 'ExPASy ENZYME',  scheduler.n = 3, entry.content.type = 'txt', urls = c(base.url = "https://enzyme.expasy.org/")))
	.self$.define('hmdb.metabolites',       properties = list(name = 'HMDB Metabolites',  scheduler.n = 3, entry.content.type = 'xml', urls = c(base.url = "http://www.hmdb.ca/")))
	.self$.define('kegg.compound',          properties = list(name = 'KEGG Compound',  scheduler.n = 3, entry.content.type = 'txt', urls = c(base.url = 'https://www.kegg.jp/', ws.url = 'http://rest.kegg.jp/', entry.page.url = 'https://www.genome.jp/dbget-bin')))
	.self$.define('kegg.enzyme',            properties = list(name = 'KEGG Enzyme',  scheduler.n = 3, entry.content.type = 'txt', urls = c(base.url = 'https://www.kegg.jp/', ws.url = 'http://rest.kegg.jp/', entry.page.url = 'https://www.genome.jp/dbget-bin')))
	.self$.define('kegg.genes',            properties = list(name = 'KEGG Genes',  scheduler.n = 3, entry.content.type = 'txt', urls = c(base.url = 'https://www.kegg.jp/', ws.url = 'http://rest.kegg.jp/', entry.page.url = 'https://www.genome.jp/dbget-bin')))
	.self$.define('kegg.module',            properties = list(name = 'KEGG Module',  scheduler.n = 3, entry.content.type = 'txt', urls = c(base.url = 'https://www.kegg.jp/', ws.url = 'http://rest.kegg.jp/', entry.page.url = 'https://www.genome.jp/dbget-bin')))
	.self$.define('kegg.pathway',           properties = list(name = 'KEGG Pathway',  scheduler.n = 3, entry.content.type = 'txt', urls = c(base.url = 'https://www.kegg.jp/', ws.url = 'http://rest.kegg.jp/', entry.page.url = 'https://www.genome.jp/dbget-bin')))
	.self$.define('kegg.reaction',          properties = list(name = 'KEGG Reaction',  scheduler.n = 3, entry.content.type = 'txt', urls = c(base.url = 'https://www.kegg.jp/', ws.url = 'http://rest.kegg.jp/', entry.page.url = 'https://www.genome.jp/dbget-bin')))
	.self$.define('lipidmaps.structure',    properties = list(name = 'LIPID MAPS Structure',  scheduler.n = 1, scheduler.t = 20, entry.content.type = 'csv', urls = c(base.url = 'http://www.lipidmaps.org/data/'))) # About access frequency, see http://www.lipidmaps.org/data/structure/programmaticaccess.html
	.self$.define('mass.csv.file',          properties = list(name = 'Mass CSV File',  entry.content.type = 'tsv', urls = character()))
	.self$.define('mass.sqlite',            properties = list(name = 'Mass SQLite',  entry.content.type = 'list', urls = character()))
	.self$.define('massbank',               properties = list(name = 'MassBank',  scheduler.n = 3, entry.content.type = 'txt', urls = c(base.url = 'https://massbank.eu/', db.tar.url = 'https://github.com/MassBank/MassBank-data/archive/master.tar.gz', prefixes.file.url = 'https://raw.githubusercontent.com/MassBank/MassBank-data/master/List_of_Contributors_Prefixes_and_Projects.md')))
	.self$.define('mirbase.mature',         properties = list(name = 'miRBase Mature',  scheduler.n = 3, entry.content.type = 'txt', urls = c(base.url = "http://www.mirbase.org/", ftp.url = "ftp://mirbase.org/pub/mirbase/CURRENT/")))
	.self$.define('ncbi.ccds',              properties = list(name = 'NCBI CCDS',  scheduler.n = 3, entry.content.type = 'html', urls = c(base.url = 'https://www.ncbi.nlm.nih.gov/CCDS/', ws.url = 'https://eutils.ncbi.nlm.nih.gov/entrez/eutils')))
	.self$.define('ncbi.gene',              properties = list(name = 'NCBI Gene',  scheduler.n = 3, entry.content.type = 'xml', urls = c(base.url = 'https://www.ncbi.nlm.nih.gov/', ws.url = 'https://eutils.ncbi.nlm.nih.gov/entrez/eutils/'))) # About access frequency, see https://www.ncbi.nlm.nih.gov/books/NBK25497/
	.self$.define('ncbi.pubchem.comp',      properties = list(name = 'PubChem Compound',  scheduler.n = 5, entry.content.type = 'xml', urls = c(base.url = 'https://pubchem.ncbi.nlm.nih.gov/', ws.url = 'https://eutils.ncbi.nlm.nih.gov/entrez/eutils/', ws2.url = 'https://pubchem.ncbi.nlm.nih.gov/rest/pug/'))) # About access frequency, see https://pubchem.ncbi.nlm.nih.gov/pug_rest/PUG_REST.html
	.self$.define('ncbi.pubchem.subst',     properties = list(name = 'PubChem Substance',  scheduler.n = 5, entry.content.type = 'xml', urls = c(base.url = 'https://pubchem.ncbi.nlm.nih.gov/', ws.url = 'https://eutils.ncbi.nlm.nih.gov/entrez/eutils/', ws2.url = 'https://pubchem.ncbi.nlm.nih.gov/rest/pug/'))) # About access frequency, see https://pubchem.ncbi.nlm.nih.gov/pug_rest/PUG_REST.html
	.self$.define('peakforest.mass',        properties = list(name = 'PeakForest Mass',  scheduler.n = 3, entry.content.type = 'json', urls = c(base.url = 'https://metabohub.peakforest.org/webapp/home', ws.url = 'https://metabohub.peakforest.org/rest/')))
	.self$.define('peakforest.compound',    properties = list(name = 'PeakForest Compound',  scheduler.n = 3, entry.content.type = 'json', urls = c(base.url = 'https://metabohub.peakforest.org/webapp/home', ws.url = 'https://metabohub.peakforest.org/rest/')))
	.self$.define('uniprot',                properties = list(name = 'UniProt',  entry.content.type = 'xml', urls = c(base.url = 'https://www.uniprot.org/uniprot/'), xml.ns = c(uniprot = "http://uniprot.org/uniprot")))
})

# Define {{{2
################################################################

BiodbDbsInfo$methods( .define = function(id, ...) {

	# Is this database already defined?
	if (id %in% names(.self$.dbs))
		.self$message('error', paste("Database \"", id, "\" has already been defined.", sep = ''))

	# Define new field
	.self$.dbs[[id]] <- BiodbDbInfo$new(parent = .self, db.class = id, ...)
})
