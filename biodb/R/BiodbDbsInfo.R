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
	.self$.define('massbank.eu')
	.self$.define('massbank.jp')
	.self$.define('mass.csv.file')
	.self$.define('mirbase.mature')
	.self$.define('ncbi.ccds')
	.self$.define('ncbi.gene')
	.self$.define('peakforest.mass')
	.self$.define('peakforest.compound')
	.self$.define('ncbi.pubchem.comp')
	.self$.define('ncbi.pubchem.subst')
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
