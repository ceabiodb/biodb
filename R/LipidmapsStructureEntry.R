# vi: fdm=marker ts=4 et cc=80 tw=80

# LipidmapsStructureEntry {{{1
################################################################################

#' @include BiodbCsvEntry.R
LipidmapsStructureEntry <- methods::setRefClass("LipidmapsStructureEntry",
    contains='BiodbCsvEntry',

# Public methods {{{1
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(...) {

    callSuper(na.strings=c('', '-'), ...)
},

# Private methods {{{1
################################################################################

# Is content correct {{{3
################################################################################

.isContentCorrect=function(content) {
    return( ! grepl("No record found", content))
},

# Parse fields step 2 {{{3
################################################################################

.parseFieldsStep2=function(parsed.content) {

    # Set synonyms 
    if (.self$hasField('SYNONYMS')) {
        v <- strsplit(.self$getFieldValue('SYNONYMS'), ';')[[1]]
        v <- sub('^ +', '', v, perl=TRUE)
        v <- sub(' +$', '', v, perl=TRUE)
        .self$setFieldValue('SYNONYMS', v)
    }

    # Synonyms
    if ('SYNONYMS' %in% names(parsed.content)) {
        v <- parsed.content[['SYNONYMS']]
        if ( ! is.na(v)) {
            v <- strsplit(v, ' *; *')[[1]]
            .self$appendFieldValue('name', v)
        }
    }
}

))
