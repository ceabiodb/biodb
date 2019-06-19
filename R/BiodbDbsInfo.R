# vi: fdm=marker ts=4 et cc=80 tw=80

# BiodbDbsInfo {{{1
################################################################################

#' A class for describing the available databases.
#'
#' The unique instance of this class is handle by the \code{\link{Biodb}} class
#' and accessed through the \code{getDbsInfo()} method.
#'
#' @seealso \code{\link{Biodb}}, \code{\link{BiodbDbInfo}}.
#'
#' @param db.id A database ID, as a character string.
#'
#' @examples
#' # Getting the base URL of a database:
#' mybiodb <- biodb::Biodb()
#' chebi.base.url <- mybiodb$getDbsInfo()$get('chebi')$getPropValSlot('urls',
#' 'base.url')
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
BiodbDbsInfo <- methods::setRefClass("BiodbDbsInfo",
    contains="BiodbChildObject",
    
# Fields {{{2
################################################################################

fields=list(
    .dbs="list"
),

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{1
################################################################################

initialize=function(...) {

    callSuper(...)

    .self$.dbs <- list()
},

# Define {{{3
################################################################################

define=function(def) {
    'Define databases from a structured object, normally loaded from a YAML
    file.'

    # Loop on all db info
    for (db in names(def))

        # Database already defined
        if (db %in% names(.self$.dbs))
            .self$.dbs[[db]]$updatePropertiesDefinition(def[[db]])

        # Define new database
        else
            .self$.dbs[[db]] <- BiodbDbInfo$new(parent=.self, db.class=db,
                                                properties=def[[db]])
},

# Get list of database IDs {{{3
################################################################################

getIds=function() {
    "Returns a character vector containing all the IDs of the defined
    databases."

    return(names(.self$.dbs))
},

# Is defined {{{3
################################################################################

isDefined=function(db.id) {
    "Returns TRUE if the specified id corresponds to a defined database."

    return(db.id %in% names(.self$.dbs))
},

# Check is defined {{{3
################################################################################

checkIsDefined=function(db.id) {
    "Throws an error if the specified id does not correspond to a defined
    database."

    if ( ! .self$isDefined(db.id))
        .self$error("Database \"", db.id, "\" is not defined.")
},

# Get {{{3
################################################################################

get=function(db.id) {
    "Returns the BiodbDbInfo instance corresponding to the specified database
    ID."

    .self$checkIsDefined(db.id)
    db <- .self$.dbs[[db.id]]
    return(db)
},

# Get all {{{3
################################################################################

getAll=function() {
    "Returns a list of all BiodbDbInfo instances."

    return(unname(.self$.dbs))
},

# Show {{{3
################################################################################

show=function() {
    cat("Biodb databases information instance.\n")
}

))
