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
    chk::chk_null_or(value, chk_string)
    
    if ( ! is.null(value))
        private$txt <- gsub(paste0('{{', tag, '}}'), value, private$txt, fixed=TRUE)
},

#' @description
#' Choose one case among a set of cases.
#' @param set  The name of the case set.
#' @param case The name of case.
choose=function(set, case) {
    chk::chk_string(set)
    chk::chk_string(case)
    
    set <- gsub('[^A-Za-z0-9]', '_', toupper(set))
    case <- gsub('[^A-Za-z0-9]', '_', toupper(case))
    caseRE <- paste0('^.*\\$\\$\\$ *CASE *', set, ' *.*\\$\\$\\$.*$')
    caseChosenRE <- paste0('^.*\\$\\$\\$ *CASE *', set, ' *',
                           case, ' *\\$\\$\\$.*$')
    caseDefaultRE <- paste0('^.*\\$\\$\\$ *CASE *', set,
                            ' *DEFAULT *\\$\\$\\$.*$')
    caseEndRE <- paste0('^.*\\$\\$\\$ *END_CASE *', set, ' *\\$\\$\\$.*$')

    while (length(starts <- grep(caseRE, private$txt)) > 0) {
        i <- starts[[1]]
        insideChosenCase <- FALSE
        foundCase <- FALSE
        while ( ! grepl(caseEndRE, private$txt[[i]])) {
            if (grepl(caseChosenRE, private$txt[[i]])) {
                private$txt <- private$txt[setdiff(seq_along(private$txt), i)]
                insideChosenCase <- TRUE
                foundCase <- TRUE
            } else if (grepl(caseDefaultRE, private$txt[[i]])) {
                private$txt <- private$txt[setdiff(seq_along(private$txt), i)]
                insideChosenCase <- ! foundCase
                foundCase <- TRUE
            } else if (grepl(caseRE, private$txt[[i]])) {
                private$txt <- private$txt[setdiff(seq_along(private$txt), i)]
                insideChosenCase <- FALSE
            } else if (insideChosenCase)
                i <- i + 1
            else {
                private$txt <- private$txt[setdiff(seq_along(private$txt), i)]
            }
        }

        # Remove END_CASE line
        private$txt <- private$txt[setdiff(seq_along(private$txt), i)]
    }
},

#' @description
#' Select or remove sections that match a name.
#' @param section The name of the section.
#' @param enable  Set to TRUE to select the section (and keep it), and FALSE to
#' remove it.
select=function(section, enable) {
    chk::chk_string(section)
    chk::chk_flag(enable)
    
    section <- gsub('[^A-Za-z0-9]', '_', toupper(section))
    sectionRE <- paste0('^.*\\$\\$\\$ *SECTION *', section, ' *\\$\\$\\$.*$')
    sectionEndRE <- paste0('^.*\\$\\$\\$ *END_SECTION *', section,
                           ' *\\$\\$\\$.*$')

    # Get all start and section sections
    starts <- grep(sectionRE, private$txt)
    ends <- grep(sectionEndRE, private$txt)
    
    # Match each start with its corresponding end (i.e.: the closest one)
    matchingEnds <- vapply(starts, function(i) head(ends[ends > i], n=1),
                           FUN.VALUE=1)

    # Remove only start and end if we keep the sections
    linesToRemove <- if (enable) c(starts, ends) else
    # Remove all lines from start to end if we remove the sections
        unlist(lapply(seq_along(starts), function(i) seq(starts[[i]],
                                                         matchingEnds[[i]])))

    # Remove lines
    private$txt <- private$txt[setdiff(seq_along(private$txt), linesToRemove)]
},

#' @description
#' Write template with replaced values to disk.
#' @param path Path to output file.
write=function(path, overwrite=FALSE) {

    if ( ! overwrite && file.exists(path))
        stop('Destination file "', path, '" already exists.')

    writeLines(private$txt, path)
}
),

private=list(
    path=NULL,
    txt=NULL,
    
loadTemplate=function() {
    private$txt <- readLines(private$path)
}
))
