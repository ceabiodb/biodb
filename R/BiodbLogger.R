#' A class for logging biodb messages either to standard stream or into a file.
#'
#' This class implements a logger for the biodb package. When creating an
#' instance of this class, you can choose to either log to a standard stream
#' (standard error or standard output) or to a file. See section Fields for a
#' list of the constructor's parameters.
#'
#' The constructor accepts the following arguments:
#'
#' file: The file path, file connection or standard stream to which you
#'       want to send biodb messages.
#'
#' mode: For a file, the mode in which you want to open the file. 'w' for
#'       writing (if a file already exists, it will be erased), and 'a' for
#'       appending (if a file already exists, logs will be appended to it).
#'
#' close.file: If set to TRUE, and `file` is a file path or a file connection,
#'             then the file connection will be closed when calling
#'             `terminate()`. If it is set to FALSE, the connection will not be
#'             closed.
#'
#' @seealso \code{\link{Biodb}} and super \code{\link{BiodbObserver}}.
#'
#' @examples
#' # Create a file logger
#' logger <- biodb::BiodbLogger(file="myfile.log")
#'
#' # Create a biodb instance
#' mybiodb <- biodb::Biodb()
#'
#' # Add the logger to the list of observers
#' mybiodb$addObservers(logger)
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @import methods
#' @include BiodbObserver.R
#' @export BiodbLogger
#' @exportClass BiodbLogger
BiodbLogger <- methods::setRefClass("BiodbLogger",
    contains='BiodbObserver',
    fields=list(
        .file='ANY',
        .close.file='logical'
        ),

methods=list(

initialize=function(file=NULL, mode='w', close.file=TRUE, ...) {

    callSuper(...)

    # Is file NA?
    if ( ! is.null(file) && (is.na(file) || ( ! is.character(file)
                             && ! methods::is(file, 'connection'))))
        stop("You must choose either a connection to a file or a file path",
              " for the \"file\" parameter")

    # File is a path => create a connection
    if (is.character(file))
        file <- file(file, open=mode)

    # Check file class type
    if ( ! is.null(file) && ! methods::is(file, 'file')
        && ! methods::is(file, 'terminal'))
        stop('Unknown class "', class(file), '" for log file.')

    # Set member field
    .self$.file <- file
    .self$.close.file <- close.file
},

terminate=function() {
    # Overrides super class' method.

    if (.self$.close.file && ! is.null(.self$.file)
        && ! methods::is(.self$.file, 'terminal'))
        close(.self$.file)
},

msg=function(type='info', msg, class=NA_character_, method=NA_character_, lvl=1)
{
    # Overrides super class' method.

    .self$checkMessageType(type)
    setlvl <- .self$getLevel(type)

    if (setlvl >= lvl) {

        # Set caller information
        caller.info <- if (is.na(class)) '' else class
        caller.info <- if (is.na(method)) caller.info
            else paste(caller.info, method, sep='::')
        if (nchar(caller.info) > 0)
            caller.info <- paste('[', caller.info, '] ', sep='')

        # Set timestamp
        timestamp <- paste('[', as.POSIXlt(Sys.time()), ']', sep='')

        # Output message
        m <- paste0('BIODB.', toupper(type), timestamp, caller.info, ': ', msg)
        if (is.null(.self$.file))
            base::message(m)
        else
            cat(m, "\n", sep='', file=.self$.file)
    }

    invisible()
}

))
