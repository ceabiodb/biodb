# vi: fdm=marker ts=4 et cc=80

#' @include BiodbCsvEntry.R

# Class declaration {{{1
################################################################################

LipidmapsStructureEntry <- methods::setRefClass("LipidmapsStructureEntry", contains='BiodbCsvEntry')

# Initialize {{{1
################################################################################

LipidmapsStructureEntry$methods( initialize=function(...) {

    callSuper(na.strings=c('', '-'), ...)
})

# Is content correct {{{1
################################################################################

LipidmapsStructureEntry$methods( .isContentCorrect=function(content) {
    return( ! grepl("No record found", content))
})

# Parse fields step 2 {{{1
################################################################################

LipidmapsStructureEntry$methods( .parseFieldsStep2=function(parsed.content) {

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
})
