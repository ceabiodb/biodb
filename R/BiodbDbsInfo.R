#' A class for describing the available databases.
#'
#' The unique instance of this class is handle by the \code{\link{Biodb}} class
#' and accessed through the \code{getDbsInfo()} method.
#'
#' @seealso \code{\link{Biodb}} and child class \code{\link{BiodbDbInfo}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Getting the entry content type of a database:
#' db.inf <- mybiodb$getDbsInfo()$get('comp.csv.file')
#' cont.type <- db.inf$getPropertyValue('entry.content.type')
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
    fields=list(
        .dbs="list"
    ),

methods=list(

initialize=function(...) {

    callSuper(...)

    .self$.dbs <- list()
},

define=function(def, package='biodb') {
    ":\n\nDefine databases from a structured object, normally loaded from a YAML
    file.
    \ndef: A named list of database definitions. The names of the list will be 
    the IDs of the databases.
    \npackage: The package to which belong the new definitions.
    \nReturned value: None.
    "

    # Loop on all db info
    for (db in names(def)) {

        dbdef <- def[[db]]
        dbdef[['package']] <- package

        # Database already defined
        if (db %in% names(.self$.dbs))
            .self$.dbs[[db]]$updatePropertiesDefinition(dbdef)

        # Define new database
        else
            .self$.dbs[[db]] <- BiodbDbInfo$new(parent=.self, db.class=db,
                                                properties=dbdef)
    }
},

getIds=function() {
    ":\n\nGets the database IDs.
    \nReturned value: A character vector containing all the IDs of the defined
    databases.
    "

    return(names(.self$.dbs))
},

isDefined=function(db.id) {
    ":\n\nTests if a database is defined.
    \ndb.id: A database ID, as a character string.
    \nReturned value: TRUE if the specified id corresponds to a defined
    database, FALSE otherwise.
    "

    return(db.id %in% names(.self$.dbs))
},

checkIsDefined=function(db.id) {
    ":\n\nChecks if a database is defined. Throws an error if the specified id
    does not correspond to a defined database.
    \ndb.id: A database ID, as a character string.
    \nReturned value: None.
    "

    if ( ! .self$isDefined(db.id))
        .self$error("Database \"", db.id, "\" is not defined.")
},

get=function(db.id) {
    ":\n\nGets information on a database.
    \ndb.id: A database ID, as a character string.
    \nReturned value: The BiodbDbInfo instance corresponding to the specified
    database ID.
    "

    .self$checkIsDefined(db.id)
    db <- .self$.dbs[[db.id]]
    return(db)
},

getAll=function() {
    ":\n\nGets informations on all databases.
    \nReturned value: A list of all BiodbDbInfo instances."

    return(unname(.self$.dbs))
},

show=function() {
    ":\n\nPrints informations about this instance, listing also all databases
    defined.
    \nReturned value: None.
    "

    cat("Biodb databases information instance.\n")
    cat("The following databases are defined:\n")
    for (id in names(.self$.dbs)) {
        cat("  ", id, ".", sep='')
        db <- .self$get(id)
        if (db$getPropertyValue('disabled'))
            cat(" DISABLED (", db$getPropertyValue('disabling.reason'),").",
                sep='')
        cat("\n")
    }
}

))
