#' A class for describing the available databases.
#'
#' The unique instance of this class is handle by the \code{\link{BiodbMain}}
#' class and accessed through the \code{getDbsInfo()} method.
#'
#' @seealso \code{\link{BiodbMain}} and child class \code{\link{BiodbDbInfo}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::newInst()
#'
#' # Getting the entry content type of a database:
#' db.inf <- mybiodb$getDbsInfo()$get('comp.csv.file')
#' cont.type <- db.inf$getPropertyValue('entry.content.type')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @import methods
#' @include BiodbDbInfo.R
#' @export BiodbDbsInfo
#' @exportClass BiodbDbsInfo
BiodbDbsInfo <- methods::setRefClass("BiodbDbsInfo",
    fields=list(
        .dbs="list",
        .cfg='ANY'
    ),

methods=list(

initialize=function(cfg) {

    chk::chk_is(cfg, 'BiodbConfig')
    .self$.dbs <- list()
    .self$.cfg <- cfg
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

        logDebug("Define connector %s.", db)
        dbdef <- def[[db]]
        dbdef[['package']] <- package

        # Database connector already defined
        if (db %in% names(.self$.dbs))
            .self$.dbs[[db]]$updatePropertiesDefinition(dbdef)

        # Define new database connector
        else
            .self$.dbs[[db]] <- BiodbDbInfo$new(db.class=db, properties=dbdef,
                cfg=.self$.cfg)
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
    \ndb.id: A character vector of database IDs.
    \nReturned value: None.
    "

    notDefined <- vapply(db.id, function(x) { ! .self$isDefined(x) },
        FUN.VALUE=TRUE)
    if (any(notDefined))
        error0("Database(s) \"", paste(db.id[notDefined], collapse=", "),
        "\" is(are) not defined.")
},

get=function(db.id=NULL, drop=TRUE) {
    ":\n\nGets information on a database.
    \ndb.id: Database IDs, as a character vector. If set to NULL, informations
    on all databases will be returned.
    \ndrop: If TRUE and only one database ID has been submitted, returns a
    single BiodbDbInfo instance instead of a list.
    \nReturned value: A list of BiodbDbInfo instances corresponding to the
    specified database IDs.
    "

    if (is.null(db.id))
        db <- .self$.dbs
    else {
        .self$checkIsDefined(db.id)
        db <- .self$.dbs[db.id]
    }

    if (drop && length(db) == 1)
        db <- db[[1]]

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
        cc <- .self$.dbs[[id]] # connector class
        cat("  ", id, ": ", cc$getPropertyValue('name'),
            " connector class", sep='')
        if (cc$hasPropSlot('urls', 'base.url'))
            cat(', using URL "', cc$getPropValSlot('urls', 'base.url'),
                '"', sep='')
        cat(".", sep='')
        db <- .self$get(id)
        if (db$getPropertyValue('disabled'))
            cat(" DISABLED (", db$getPropertyValue('disabling.reason'),").",
                sep='')
        cat("\n")
    }
}

))
