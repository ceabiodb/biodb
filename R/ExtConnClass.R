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
#' @param connType  The type of connector class to implement.
#' @return A new instance.
initialize=function(path, dbName, dbTitle=NULL,
                    connType=c('plain', 'compound', 'mass'),
                    editable=FALSE, writable=FALSE
                    ) {
    chk::chk_dir(path)
    chk::chk_string(dbName)
    chk::chk_null_or(dbTitle, chk::chk_string)
    chk::chk_flag(editable)
    chk::chk_flag(writable)
	connType <- match.arg(connType)
    
    private$path <- normalizePath(path)
    private$dbName <- dbName
    private$dbTitle <- dbTitle
    private$connType <- connType
    private$editable <- editable
    private$writable <- writable
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
    templ$select('editable', private$editable)
    templ$select('writable', private$writable)
    templ$write(private$getConnClassFile())
}

),
                            
private=list(
    path=NULL,
    dbName=NULL,
    dbTitle=NULL,
    connType=NULL,
    editable=NULL,
    writable=NULL,
 
getRFolder=function() {
    rFolder <- file.path(private$path, 'R')
    if ( ! dir.exists(rFolder))
        dir.create(rFolder)
    return(rFolder)
},

getConnClassFile=function() {
    filename <- paste0(getConnClassName(private$dbName), '.R')
    return(file.path(private$getRFolder(), filename))
}
))
