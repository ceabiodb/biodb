# vi: fdm=marker ts=4 et cc=80 tw=80

# BiodbCsvEntry {{{1
################################################################################

#' @include BiodbEntry.R
BiodbCsvEntry <- methods::setRefClass("BiodbCsvEntry",
    contains='BiodbEntry',
    fields=list(
                .sep='character',
                .na.strings='character'),

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(sep=',', na.strings='NA', ...) {

    callSuper(...)
    .self$.abstractClass('BiodbCsvEntry')

    .self$.sep <- sep
    .self$.na.strings <- na.strings
},

# Private methods {{{2
################################################################################

# Do parse content {{{3
################################################################################

.doParseContent=function(content) {

    # Read all CSV file, including header line, into a data frame. The header
    # line will then be the first line. This is to avoid first column to be
    # interpretated as row names by read.table in case the header line contains
    # one less field than the second line.
    df <- read.table(text=content, header=FALSE, row.names=NULL, sep=.self$.sep,
                     quote='', stringsAsFactors=FALSE,
                     na.strings=.self$.na.strings, fill=TRUE, check.names=FALSE,
                     comment.char='')

    # Now name the columns
    if (nrow(df) >= 1) {

        # Remove unnamed columns
        df <- df[, ! is.na(df[1, ])]

        # Set colnames
        colnames(df) <- df[1, ]
        df <- df[seq(nrow(df)) != 1, ]
    }

    return(df)
},

# Is parsed content correct {{{3
################################################################################

.isParsedContentCorrect=function(parsed.content) {
    return(nrow(parsed.content) > 0)
},

# Parse fields step 1 {{{3
################################################################################

.parseFieldsStep1=function(parsed.content) {

    cfg <- .self$getBiodb()$getConfig()
    
    # Get parsing expressions
    parsing.expr <- .self$getParent()$getPropertyValue('parsing.expr')

    # Loop on all expressions
    for (field in names(parsing.expr)) {

        # Is field in columns?
        if (parsing.expr[[field]] %in% colnames(parsed.content)) {

            # Get field definition
            field.def <- .self$getBiodb()$getEntryFields()$get(field)

            # Get value
            v <- parsed.content[[parsing.expr[[field]]]]

            # Is value considered NA?
            if ( ! is.null(.self$.na.strings) && length(.self$.na.strings >= 1)
                && ! all(is.na(.self$.na.strings)))
                v[v %in% .self$.na.strings] <- NA

            # Remove NA values
            v <- v[ ! is.na(v)]

            # Remove duplicated values
            v <- v[ ! duplicated(v)]

            # Split
            if (field.def$hasCardMany() && length(v) == 1)
                v <- strsplit(v, cfg$get('multival.field.sep'))[[1]]

            # Set value
            if (length(v) > 0 && any( ! is.na(v)))
                .self$setFieldValue(field, v)
        }
    }
}

))
