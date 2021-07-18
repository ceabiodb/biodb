#' Entry class for content in CSV format.
#'
#' This is an abstract class for handling database entries whose content is in
#' CSV format.
#'
#' @seealso Super class \code{\link{BiodbEntry}}.
#'
#' @examples
#' # Create a concrete entry class inheriting from CSV class:
#' MyEntry <- R6::R6Class("MyEntry", inherit=biodb::BiodbCsvEntry)
#'
#' @include BiodbEntry.R
#' @export
BiodbCsvEntry <- R6::R6Class("BiodbCsvEntry",
inherit=BiodbEntry,

public=list(

#' @description
#' New instance initializer. Entry objects must not be created directly.
#' Instead, they are retrieved through the connector instances.
#' @param sep The separator to use in CSV files.
#' @param na.strings The strings to recognize as NA values. This is a character
#' vector.
#' @param quotes The characters to recognize as quotes. This is a single
#' character value.
#' @param ... The remaining arguments will be passed to the super class
#' initializer.
#' @return Nothing.
initialize=function(sep=',', na.strings='NA', quotes='', ...) {

    super$initialize(...)
    abstractClass('BiodbCsvEntry', self)

    chk::chk_string(sep)
    chk::chk_character(na.strings)
    chk::chk_string(quotes)
    private$sep <- sep
    private$na.strings <- na.strings
    private$quotes <- quotes

    return(invisible(NULL))
}
),

private=list(
    sep=NULL,
    na.strings=NULL,
    quotes=NULL
,
doParseContent=function(content) {

    # Read all CSV file, including header line, into a data frame. The
    # header line will then be the first line. This is to avoid first
    # column to be interpreted as row names by read.table in case the
    # header line contains one less field than the second line.
    df <- read.table(text=content, header=FALSE, row.names=NULL,
        sep=private$sep, quote=private$quotes, stringsAsFactors=FALSE,
        na.strings=private$na.strings, fill=TRUE, check.names=FALSE,
        comment.char='')

    # Now name the columns
    if (nrow(df) >= 1) {

        if (all(is.na(df[1, ])) || ncol(df) == 0) # No column names
            df <- data.frame()
        else {
            # Remove unnamed columns
            df <- df[, ! is.na(df[1, ])]

            # Set colnames
            colnames(df) <- df[1, ]
            df <- df[seq(nrow(df)) != 1, ]
        }
    }

    return(df)
},

doCheckParsedContent=function(parsed.content) {
    return(nrow(parsed.content) > 0)
}

,doParseFieldsStep1=function(parsed.content) {

    cfg <- self$getBiodb()$getConfig()

    # Get parsing expressions
    parsing.expr <- self$getParent()$getPropertyValue('parsing.expr')

    # Loop on all expressions
    for (field in names(parsing.expr)) {

        # Is field in columns?
        if (parsing.expr[[field]] %in% colnames(parsed.content)) {

            # Get field definition
            field.def <- self$getBiodb()$getEntryFields()$get(field)

            # Get value
            v <- parsed.content[[parsing.expr[[field]]]]

            # Is value considered NA?
            if ( ! is.null(private$na.strings)
                && length(private$na.strings >= 1)
                && ! all(is.na(private$na.strings)))
                v[v %in% private$na.strings] <- NA

            # Remove NA values
            v <- v[ ! is.na(v)]

            # Remove duplicated values
            if (field.def$forbidsDuplicates() || field.def$hasCardOne())
                v <- v[ ! duplicated(v)]

            # Split
            if (field.def$hasCardMany() && length(v) == 1)
                v <- strsplit(v, cfg$get('multival.field.sep'))[[1]]

            # Set value
            if (length(v) > 0 && any( ! is.na(v)))
                self$setFieldValue(field, v)
        }
    }
}
))
