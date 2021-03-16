#' File template class.
#'
#' @description
#' A class for reading a file template, replacing tags inside, and writing the
#' results in an output file.
#'
#' @import R6
#' @import chk
FileTemplate <- R6::R6Class('FileTemplate',

public=list(

#' @description
#' Constructor
#' @param path  The path to the template file.
#' @return A new instance.
initialize=function(path) {
    chk::chk_file(path)
    
    private$path <- path
    private$loadTemplate()
},

#' @description
#' Replace a tag by its value inside the template file.
#' @param tag   The tag to replace.
#' @param value The value to replace the tag with.
replace=function(tag, value) {
    chk::chk_string(tag)
    chk::chk_string(value)
    
    private$txt <- gsub(paste0('{{', tag, '}}'), value, private$txt, fixed=TRUE)
},

#' @description
#' Write template with replaced values to disk.
#' @param path Path to output file.
write=function(path) {
    chk::chk_false(chk::vld_file(path))
    
    write(private$txt, file=path)
}
),

private=list(
    path=NULL,
    txt=NULL,
    
loadTemplate=function() {
    private$txt <- readChar(private$path, file.info(private$path)$size)
}
))
