# vi: fdm=marker

# Class declaration {{{1
################################################################

#' A class for describing the available databases.
#'
#' @seealso \code{\link{Biodb}}, \code{\link{BiodbDbInfo}}.
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

# Initialize databases information {{{1
################################################################

BiodbDbsInfo$methods( .initDbsInfo = function() {
	.self$.define('chebi')
	.self$.define('chemspider')
	.self$.define('expasy.enzyme')
	.self$.define('hmdb.metabolite')
	.self$.define('kegg.compound')
	.self$.define('lipidmaps.structure')
	.self$.define('mass.csv.file')
#	.self$.define('massbank.eu') # Turn Off Massbank Europe, does not work very well when searching for spectra through API.
	.self$.define('massbank.jp')
	.self$.define('mirbase.mature')
	.self$.define('ncbi.ccds')
	.self$.define('ncbi.gene')
	.self$.define('ncbi.pubchem.comp')
	.self$.define('ncbi.pubchem.subst')
	.self$.define('peakforest.mass',        base.url = 'https://rest.peakforest.org/')
	.self$.define('peakforest.compound',    base.url = 'https://rest.peakforest.org/')
	.self$.define('uniprot')
})

# Define {{{1
################################################################

BiodbDbsInfo$methods( .define = function(name, ...) {

	# Is this database already defined?
	if (name %in% names(.self$.dbs))
		.self$message(MSG.ERROR, paste("Database \"", name, "\" has already been defined.", sep = ''))

	# Define new field
	.self$.dbs[[name]] <- BiodbDbInfo$new(parent = .self, name = name, ...)
})

# Get list of database IDs
################################################################

BiodbDbsInfo$methods( getIds = function() {
	return(names(.self$.dbs))
})

# Is defined {{{1
################################################################

BiodbDbsInfo$methods( isDefined = function(name) {
	return(name %in% names(.self$.dbs))
})

# Check is defined {{{1
################################################################

BiodbDbsInfo$methods( checkIsDefined = function(name) {
	if ( ! .self$isDefined(name))
		.self$message(MSG.ERROR, paste("Database \"", name, "\" is not defined.", sep = ''))
})

# Get {{{1
################################################################

BiodbDbsInfo$methods( get = function(name) {
	.self$checkIsDefined(name)
	db <- .self$.dbs[[name]]
	return(db)
})
