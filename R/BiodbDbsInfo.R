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
#' @import R6
#' @include BiodbDbInfo.R
#' @export
BiodbDbsInfo <- R6::R6Class("BiodbDbsInfo",

public=list(

#' @description
#' New instance initializer. The class must not be instantiated directly.
#' Instead, access the BiodbDbsInfo instance through the BiodbMain instance
#' using the getDbsInfo() method.
#' @param cfg The BiodbConfig instance.
#' @return Nothing.
initialize=function(cfg) {

    chk::chk_is(cfg, 'BiodbConfig')
    private$dbs <- list()
    private$cfg <- cfg

    return(invisible(NULL))
},

#' @description
#' Define databases from a structured object, normally loaded from a YAML
#' file.
#' @param def A named list of database definitions. The names of the list
#' will be the IDs of the databases.
#' @param package The package to which belong the new definitions.
#' @return Nothing.
define=function(def, package='biodb') {

    # Loop on all db info
    for (db in names(def)) {

        logDebug("Define connector %s.", db)
        dbdef <- def[[db]]
        dbdef[['package']] <- package

        # Database connector already defined
        if (db %in% names(private$dbs))
            private$dbs[[db]]$updatePropertiesDefinition(dbdef)

        # Define new database connector
        else
            private$dbs[[db]] <- BiodbDbInfo$new(db.class=db, properties=dbdef,
                cfg=private$cfg)
    }

    return(invisible(NULL))
},

#' @description
#' Gets the database IDs.
#' @return A character vector containing all the IDs of the defined
#'     databases.
getIds=function() {

    return(names(private$dbs))
},

#' @description
#' Tests if a database is defined.
#' @param db.id A database ID, as a character string.
#' @return TRUE if the specified id corresponds to a defined
#'     database, FALSE otherwise.
isDefined=function(db.id) {

    return(db.id %in% names(private$dbs))
},

#' @description
#' Checks if a database is defined. Throws an error if the specified id
#'     does not correspond to a defined database.
#' @param db.id A character vector of database IDs.
#' @return Nothing.
checkIsDefined=function(db.id) {

    notDefined <- vapply(db.id, function(x) { ! self$isDefined(x) },
        FUN.VALUE=TRUE)
    if (any(notDefined))
        error0("Database(s) \"", paste(db.id[notDefined], collapse=", "),
        "\" is(are) not defined.")

    return(invisible(NULL))
},

#' @description
#' Gets information on a database.
#' @param db.id Database IDs, as a character vector. If set to NULL,
#' informations on all databases will be returned.
#' @param drop If TRUE and only one database ID has been submitted,
#' returns a single BiodbDbInfo instance instead of a list.
#' @return A list of BiodbDbInfo instances corresponding to the specified
#' database IDs.
get=function(db.id=NULL, drop=TRUE) {

    if (is.null(db.id))
        db <- private$dbs
    else {
        self$checkIsDefined(db.id)
        db <- private$dbs[db.id]
    }

    if (drop && length(db) == 1)
        db <- db[[1]]

    return(db)
},

#' @description
#' Gets informations on all databases.
#' @return A list of all BiodbDbInfo instances."
getAll=function() {
    return(unname(private$dbs))
},

#' @description
#' Prints informations about this instance, listing also all databases
#'     defined.
#' @return Nothing.
print=function() {

    cat("Biodb databases information instance.\n")
    cat("The following databases are defined:\n")
    for (id in names(private$dbs)) {
        cc <- private$dbs[[id]] # connector class
        cat("  ", id, ": ", cc$getPropertyValue('name'),
            " connector class", sep='')
        if (cc$hasPropSlot('urls', 'base.url'))
            cat(', using URL "', cc$getPropValSlot('urls', 'base.url'),
                '"', sep='')
        cat(".", sep='')
        db <- self$get(id)
        if (db$getPropertyValue('disabled'))
            cat(" DISABLED (", db$getPropertyValue('disabling.reason'),").",
                sep='')
        cat("\n")
    }

    return(invisible(NULL))
}
),

private=list(
    dbs=NULL,
    cfg=NULL
))
