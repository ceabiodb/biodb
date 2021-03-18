#' Extension connector clas
#'
#' @description
#' A class for generating a new connector class.
#'
#' @details
#' This class generates a new connector class from given parameters.
#' The new class can inherit directly from \code{BiodbConn} or
#' \code{BiodbCompounddbConn} or \code{BiodbMassdbConn}.
#' It can also be editable and/or writable.
#'
#' @import R6
#' @import chk
#' @export
ExtConnClass <- R6::R6Class('ExtConnClass',

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
#' @param remote    Set to TRUE if the database to connect to is not local.
#' @param downloadable  Set to TRUE if the database needs to be downloaded or
#' offers this possiblity.
#' @param editable  Set to TRUE to allow this connector to create new entries in
#' memory.
#' @param writable  Set to TRUE to enable this connector to write into the
#' database.
#' @return A new instance.
initialize=function(path, dbName, dbTitle=NULL,
                    connType=c('plain', 'compound', 'mass'),
                    editable=FALSE, writable=FALSE,
                    remote=FALSE, downloadable=FALSE
                    ) {
    chk::chk_dir(path)
    chk::chk_string(dbName)
    chk::chk_null_or(dbTitle, chk::chk_string)
    chk::chk_flag(downloadable)
    chk::chk_flag(editable)
    chk::chk_flag(writable)
    chk::chk_flag(remote)
	connType <- match.arg(connType)
    
    private$path <- normalizePath(path)
    private$dbName <- dbName
    private$dbTitle <- dbTitle
    private$connType <- connType
    private$editable <- editable
    private$writable <- writable
    private$remote <- remote
    private$downloadable <- downloadable
},

#' @description
#' Generates the connector class.
generate=function() {
    templ <- FileTemplate$new(system.file('templates', 'Conn.R',
                                          package='biodb'))
    templ$replace('dbName', private$dbName)
    templ$replace('connClass', getConnClassName(private$dbName))
    if ( ! is.null(private$dbTitle))
        templ$replace('dbTitle', private$dbTitle)
    templ$choose('mother.class', private$connType)
    templ$select('remote', private$remote)
    templ$select('downloadable', private$downloadable)
    templ$select('editable', private$editable)
    templ$select('writable', private$writable)
    templ$write(getConnClassFile(private$path, private$dbName))
}

),
                            
private=list(
    path=NULL
    ,dbName=NULL
    ,dbTitle=NULL
    ,connType=NULL
    ,editable=NULL
    ,writable=NULL
    ,remote=NULL
    ,downloadable=NULL
))
