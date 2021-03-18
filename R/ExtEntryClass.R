#' Extension entry class
#'
#' @description
#' A class for generating a new entry class.
#'
#' @details
#' This class generates a new entry class from given parameters.
#' The new class can inherit directly from \code{BiodbEntry} or
#' from one of its sub-classes: \code{BiodbCsvEntry}, \code{BiodbHtmlEntry}, ...
#'
#' @import R6
#' @import chk
#' @export
ExtEntryClass <- R6::R6Class('ExtEntryClass',

public=list(
         
#' @description
#' Constructor
#' @param path      The path to the package folder.
#' @param dbName    The name of the database (in biodb format "my.db.name"),
#' that will be used in "definitions.yml" file and for connector and entry
#' classes.
#' @param entryType  The type of entry class to implement.
#' @return A new instance.
initialize=function(path, dbName, dbTitle=NULL,
                    entryType=c('plain', 'csv', 'html', 'json', 'list', 'sdf',
                                'txt', 'xml')
                    ) {
    chk::chk_dir(path)
    chk::chk_string(dbName)
    chk::chk_null_or(dbTitle, chk::chk_string)
	entryType <- match.arg(entryType)
    
    private$path <- normalizePath(path)
    private$dbName <- dbName
    private$dbTitle <- dbTitle
    private$entryType <- entryType
}

#' @description
#' Generates the connector class.
,generate=function() {
    templ <- FileTemplate$new(system.file('templates', 'Entry.R',
                                          package='biodb'))
    templ$replace('dbName', private$dbName)
    templ$replace('connClass', getConnClassName(private$dbName))
    templ$replace('entryClass', getEntryClassName(private$dbName))
    if ( ! is.null(private$dbTitle))
        templ$replace('dbTitle', private$dbTitle)
    templ$choose('mother.class', private$entryType)
    templ$write(getEntryClassFile(private$path, private$dbName))
}
),
                            
private=list(
    path=NULL
    ,dbName=NULL
    ,dbTitle=NULL
    ,entryType=NULL
 
))
