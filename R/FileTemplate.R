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
#' Initializer.
#' @param path  The path to the template file.
#' @return Nothing.
initialize=function(path) {
    chk::chk_file(path)
    
    private$path <- path
    private$loadTemplate()

    return(invisible(NULL))
},

#' @description
#' Replace a tag by its value inside the template file.
#' @param tag   The tag to replace.
#' @param value The value to replace the tag with.
#' @return invisible(self) for chaining method calls.
replace=function(tag, value) {
    chk::chk_string(tag)
    chk::chk_null_or(value, vld=chk::vld_string)
    
    if ( ! is.null(value))
        private$txt <- gsub(paste0('{{', tag, '}}'),
            value, private$txt, fixed=TRUE)
    
    return(invisible(self))
},

#' @description
#' Choose one case among a set of cases.
#' @param set  The name of the case set.
#' @param case The name of case.
#' @return invisible(self) for chaining method calls.
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
    
    return(invisible(self))
},

#' @description
#' Select or remove sections that match a name.
#' @param section The name of the section.
#' @param enable  Set to TRUE to select the section (and keep it), and FALSE to
#' remove it.
#' @return invisible(self) for chaining method calls.
select=function(section, enable) {
    chk::chk_string(section)
    chk::chk_flag(enable)
    
    section <- gsub('[^A-Za-z0-9]', '_', toupper(section))
    ifRE <- paste0('^.*\\$\\$\\$ *(SECTION|IF) *', section, ' *\\$\\$\\$.*$')
    elseRE <- paste0('^.*\\$\\$\\$ *ELSE *', section, ' *\\$\\$\\$.*$')
    endRE <- paste0('^.*\\$\\$\\$ *END_(SECTION|IF) *', section,
        ' *\\$\\$\\$.*$')

    # Get all start and section sections
    ifs <- grep(ifRE, private$txt)
    elses <- grep(elseRE, private$txt)
    ends <- grep(endRE, private$txt)
    
    # Match each start with its corresponding end (i.e.: the closest one)
    elseEnds <- sort(c(elses, ends))
    ifMatchingEnds <- vapply(ifs, function(i) head(elseEnds[elseEnds > i], n=1),
        FUN.VALUE=1)
    elseMatchingEnds <- vapply(elses, function(i) head(ends[ends > i], n=1),
        FUN.VALUE=1)

    # Select lines to remove
    if (enable) {
        linesToRemove <- c(ifs, ifMatchingEnds,
            unlist(lapply(seq_along(elses), function(i)
            seq(elses[[i]], elseMatchingEnds[[i]]))))
    } else {
        linesToRemove <- c(elses, elseMatchingEnds,
            unlist(lapply(seq_along(ifs), function(i)
            seq(ifs[[i]], ifMatchingEnds[[i]]))))
    }

    # Remove lines
    private$txt <- private$txt[setdiff(seq_along(private$txt), linesToRemove)]
    
    return(invisible(self))
}

#' @description
#' Write template with replaced values to disk.
#' @param path Path to output file.
#' @param overwrite If set to FALSE and the destination file already exists, a
#' message is thrown. Otherwise writes into the destination.
#' @param checkRemainingTags If set to TRUE, checks first, before writing, if
#' there any remaining tags that have not been processed. A warning is thrown
#' for each found tag.
#' @return Nothing.
,write=function(path, overwrite=FALSE, checkRemainingTags=TRUE) {

    if (checkRemainingTags)
        private$checkRemainingTags()

    if ( ! overwrite && file.exists(path))
        stop('Destination file "', path, '" already exists.')

    writeLines(private$txt, path)
    
    return(invisible(NULL))
}

#' @description
#' Get the lines of the templates.
#' @return A vector containing the lines of the templates.
,getLines=function() {
    return(private$txt)
}
),

private=list(
    path=NULL
    ,txt=NULL
    
,loadTemplate=function() {
    private$txt <- readLines(private$path)
}

,checkRemainingTags=function() {
    
    # Search for {{...}} tags
    tags <- unlist(lapply(stringr::str_match_all(private$txt,
        '\\{\\{([^}]+)\\}\\}'), function(x) x[,2]))
    
    # Search for $$$ ... $$$ tags
    tagLines <- stringr::str_match(private$txt,
        '^.*\\$\\$\\$ *[^ ]+ +([^ ]+).*\\$\\$\\$.*$')
    tags <- c(tags, tagLines[,2])
    
    # Sort and remove duplicates
    tags <- tags[ ! is.na(tags)]
    tags <- unique(sort(tags))
    
    # Print warning
    if (length(tags) > 0)
        warning('The following tags were not replaced inside "', private$path,
            '": ', paste(tags, collapse=', '), '.')
}
))
