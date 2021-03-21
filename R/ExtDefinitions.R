#' Extension defintions file class
#'
#' @description
#' A class for generating the definitions.yml file of a new extension package.
#'
#' @details
#' This class generates the definitions.yml of a new extension package, needed
#' for definining the new connector.
#'
#' @import R6
#' @import chk
#' @include ExtFileGenerator.R
#' @export
ExtDefinitions <- R6::R6Class('ExtDefinitions',
                              
inherit=ExtFileGenerator,

public=list(
         
#' @description
#' Constructor
#' @param path      The path to the package folder.
#' @param dbName    The name of the database (in biodb format "my.db.name"),
#' that will be used in "definitions.yml" file and for connector and entry
#' classes.
#' @param dbTitle   The official name of the database (e.g.: HMDB, UniProtKB,
#' KEGG).
#' @param connType  The type of connector class to implement.
#' @param entryType The type of entry class to implement.
#' @param remote    Set to TRUE if the database to connect to is not local.
#' @param downloadable  Set to TRUE if the database needs to be downloaded or
#' offers this possiblity.
initialize=function(dbName=NULL, dbTitle=NULL,
                    connType=c('plain', 'compound', 'mass'),
                    entryType=c('plain', 'csv', 'html', 'json', 'list', 'sdf',
                                'txt', 'xml'),
                    remote=FALSE, downloadable=FALSE, ...
                    ) {
    super$initialize(template='definitions.yml', folder='inst',
                     filename='definitions.yml', ...)
    chk::chk_null_or(dbName, chk::chk_string)
    chk::chk_null_or(dbTitle, chk::chk_string)
    chk::chk_flag(remote)
    chk::chk_flag(downloadable)
    connType <- match.arg(connType)
    entryType <- match.arg(entryType)

    private$dbName <- dbName
    private$dbTitle <- dbTitle
    private$connType <- connType
    private$entryType <- entryType
    private$remote <- remote
    private$downloadable <- downloadable
}

#' @description
#' Generates the definitions file for the specified package.
,generate=function() {
    temp <- FileTemplate$new(private$getTemplateFile())
    temp$replace('dbName', private$dbName)
    temp$replace('dbTitle', private$dbTitle)
    temp$choose('conn.type', private$connType)
    temp$choose('entry.type', private$entryType)
    temp$select('remote', private$remote)
    temp$select('downloadable', private$downloadable)
    temp$write(private$getDstFile())
}
),

private=list(
    dbName=NULL
    ,dbTitle=NULL
    ,connType=NULL
    ,entryType=NULL
    ,remote=NULL
    ,downloadable=NULL
))
