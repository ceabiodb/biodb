#' A class for generating a DESCRIPTION file for an extension package.
#'
#' This class generates a DESCRIPTION for a biodb extension package.
#'
#' @param path     The path to the package folder.
#' @param dbName   The name of the database (in biodb format "my.db.name"),
#' that will be used in "definitions.yml" file and for connector and entry
#' classe.
#' @param newPkg   Set to TRUE if the package is not yet on Bioconductor.
#' @param rcpp     Set to TRUE to enable Rcpp C/C++ code inside the package.
#'
#' @examples
#'
#'
#' @import methods
#' @import chk
#' @import desc
#' @export ExtDescriptionFile
#' @exportClass ExtDescriptionFile
ExtDescriptionFile <- methods::setRefClass('ExtDescriptionFile',
    fields=list(
        path='character',
        dbName='ANY',
        newPkg='logical',
        rcpp='logical'
    ),

methods=list(

initialize=function(path, dbName=NULL, newPkg=FALSE, rcpp=FALSE) {
    chk::chk_dir(path)
    chk::chk_null_or(dbName, chk::chk_string)
    chk::chk_flag(newPkg)
    chk::chk_flag(rcpp)

    .self$path <- normalizePath(path)
    .self$dbName <- dbName
    .self$newPkg <- newPkg
    .self$rcpp <- rcpp
},

getPkgName=function() {
    return(basename(.self$path))
},

setNameTitleDesc=function(descFile) {
    descFile$set("Package", .self$getPkgName())
    descFile$set(Package=.self$getPkgName())
    descFile$set(Title=paste(.self$getPkgName(),
                             'a library for connecting to a/the ... database'))
    descFile$set(Description=paste('The',
                    .self$getPkgName(), 'library is an extension of',
                    'the biodb framework package, that provides access',
                    'to a/the ... database. It allows to retrieve',
                    'entries by their accession number, and ...'))
},

setAuthors=function(descFile) {
    descFile$add_author("Firstname1", "Lastname1", email="your@e.mail",
                        role=c("aut", "cre"))
    descFile$add_author("Firstname2", "Lastname2", email="another@e.mail",
                        role=c("ctb"))
},
    
setDependencies=function(descFile) {

    descFile$set_dep('R', type='Depends', version='>= 4.0')
    descFile$set_dep('methods', type='Imports')
    descFile$set_dep('biodb', type='Imports', version='>= 0.99.0')
    descFile$set_dep('testthat', type='Suggests', version='>= 2.0.0')
    descFile$set_dep('BiocStyle', type='Suggests')
    descFile$set_dep('roxygen2', type='Suggests')
    descFile$set_dep('devtools', type='Suggests')
    descFile$set_dep('knitr', type='Suggests')
    descFile$set_dep('rmarkdown', type='Suggests')
    descFile$set_dep('covr', type='Suggests')
    if (.self$rcpp) {
        descFile$set_dep('Rcpp', type='Imports')
        descFile$set_dep('Rcpp', type='LinkingTo')
        descFile$set_dep('testthat', type='LinkingTo')
        descFile$set(NeedsCompilation='yes')
    }
},

setCollateFiles=function(descFile) {

    if ( ! is.null(.self$dbName)) {
        connFile <- paste0(getConnClassName(.self$dbName), '.R')
        entryFile <- paste0(getEntryClassName(.self$dbName), '.R')
        descFile$add_to_collate(c(connFile, entryFile, 'package.R'))
        if (.self$rcpp)
            descFile$add_to_collate('RcppExports.R')
    }
},

generate=function() {
    ":\n\nGenerates the DESCRIPTION file inside the package folder.
    \nReturned value: None.
    "
    
    # Create new file
    descFile <- desc::description$new("!new")
    
    # Remove some fields
    descFile$del(c('Authors@R', 'Maintainer', 'URL', 'BugReports'))
    
    # Set fields
    .self$setNameTitleDesc(descFile)
    if (.self$newPkg)
        descFile$set(Version='0.99.0')
    descFile$set(License='AGPL-3')
    .self$setAuthors(descFile)
    .self$setDependencies(descFile)
    .self$setCollateFiles(descFile)

    # Write file
    descFile$write(file=file.path(.self$path, 'DESCRIPTION'))
}

))
