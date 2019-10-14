# vi: fdm=marker ts=4 et cc=80 tw=80

# MsgAcknowledger observer class {{{1
################################################################################

#' A class for acknowledging messages during tests.
#'
#' This observer is used to call a testthat::expect_*() method each time a
#' message is received. This is used when running tests on Travis-CI, so Travis
#' does not stop tests because no change is detected in output.
#'
#' @import methods
#' @include BiodbObserver.R
#' @export MsgAcknowledger
#' @exportClass MsgAcknowledger
MsgAcknowledger <- methods::setRefClass('MsgAcknowledger',
    contains = 'BiodbObserver',
    fields = list(
        .last.index = 'numeric'
        ),

    methods=list(

# Initialize {{{2
################################################################

initialize=function(...) {

    callSuper(...)

    .last.index <<- 0
},

# Msg {{{2
################################################################

msg=function(type='info', msg, class=NA_character_, method=NA_character_,
               lvl=1) {
    testthat::expect_is(msg, 'character')
},

# Progress {{{2
################################################################

progress=function(type='info', msg, index, first, total=NA_character_,
                    lvl=1L, laptime=10L) {

    .self$checkMessageType(type)
    testthat::expect_is(msg, 'character')
    testthat::expect_length(msg, 1)
    testthat::expect_true(msg != '')

    if (first)
        .self$.last.index[msg] <- index - 1

    testthat::expect_true(msg %in% names(.self$.last.index))
    testthat::expect_true(index > .self$.last.index[[msg]],
                        paste0("Index ", index, " is not greater than last ",
                               "index ", .self$.last.index[msg], ' for progress ',
                               'message "', msg, '", with total ', total, '.'))
    if ( ! is.na(total))
        testthat::expect_true(index <= total,
                             paste0("Index ", index, ' is greater than total ',
                                    total, ' for progress message "', msg,
                                    '".'))

    .self$.last.index[msg] <- index
}

))


# MsgRecorder observer class {{{1
################################################################################

#' A class for recording messages during tests.
#'
#' The main purpose of this class is to give access to last sent messages of the
#' different types: "error", "warning", "caution", "info" and "debug".
#'
#' @import methods
#' @include BiodbObserver.R
#' @export MsgRecorder
#' @exportClass MsgRecorder
MsgRecorder <- methods::setRefClass("MsgRecorder",
    contains = "BiodbObserver",
    fields = list(
                  .msgs='character',
                  .msgs.by.type='list'
                  ),
    methods=list(

# Initialize {{{2
################################################################################

initialize=function(...) {
    .msgs <<- character()
    .msgs.by.type <<- list()
},

# Msg {{{2
################################################################################

msg=function(type='info', msg, class=NA_character_, method=NA_character_,
             lvl=1) {
    .msgs <<- c(.self$.msgs, msg)
    .self$.msgs.by.type[[type]] <- c(.self$.msgs.by.type[[type]], msg)
},

# hasMsgs {{{2
################################################################################

hasMsgs=function(type = NULL) {

    f = FALSE

    if (is.null(type))
        f = (length(.self$.msg) > 0)
    else
        f = if (type %in% names(.self$.msgs.by.type)) (length(.self$.msgs.by.type[[type]])) else FALSE

    return(f)
},

# lastMsg {{{2
################################################################################

lastMsg = function() {

    m <- NA_character_

    i <- length(.self$.msgs)
    if (i > 0)
        m <- .self$.msgs[[i]]

    return(m)
},

# getLastMsgByType {{{2
################################################################################

getLastMsgByType = function(type) {
    m = NULL
    if (type %in% names(.self$.msgs.by.type)) {
        m = .self$.msgs.by.type[[type]]
        m = m[[length(m)]]
    }
    return(m)
},

# getMsgsByType {{{2
################################################################################

getMsgsByType = function(type) {
    msgs = character()

    if ( ! is.null(type) && type %in% names(.self$.msgs.by.type))
        msgs = .self$.msgs.by.type[[type]]

    return(msgs)
},

# clearMessages {{{2
################################################################################

clearMessages = function() {
    .msgs <<- character()
    .msgs.by.type <<- list()
}

))

# Set test context {{{1
################################################################################

#' Set text context.
#'
#' Define a context for tests using testthat framework.
#'
#' @export
setTestContext <- function(biodb, text) {

    # Set testthat context
    testthat::context(text)

    # Print banner in log file
    biodb$info("")
    biodb$info(paste(rep('*', 80), collapse=''))
    biodb$info(paste("Test context", text, sep = " - "))
    biodb$info(paste(rep('*', 80), collapse=''))
    biodb$info("")
}

# Test that {{{1
################################################################

#' @export
testThat  <- function(msg, fct, biodb=NULL, obs=NULL, conn=NULL) {

    # Get biodb instance
    if ( ! is.null(biodb) && ! methods::is(biodb, 'Biodb'))
        stop("`biodb` parameter must be a rightful biodb::Biodb instance.")
    if ( ! is.null(conn) && ! methods::is(conn, 'BiodbConn'))
        stop("`conn` parameter must be a rightful biodb::BiodbConn instance.")
    bdb <- if (is.null(conn)) biodb else conn$getBiodb()
    if (is.null(bdb))
        stop("You must at least set one of `biodb` or `conn` parameter when",
             " calling testThat().")

    # Get function name
    if (methods::is(fct, 'function'))
        fname <- deparse(substitute(fct))
    else
        fname <- fct

    # Get list of test functions to run
    if (bdb$getConfig()$isDefined('test.functions')) {
        functions <- strsplit(bdb$getConfig()$get('test.functions'), ',')[[1]]
        runFct <- fname %in% functions
    }
    else
        runFct <- TRUE

    if (runFct) {

        # Send message to logger
        bdb$info('')
        bdb$info(paste('Running test function ', fname, ' ("', msg, '").'))
        bdb$info(paste(rep('-', 80), collapse=''))
        bdb$info('')

        # Call test function
        if ( ! is.null(biodb) && ! is.null(obs))
            testthat::test_that(msg, do.call(fct, list(biodb = biodb, obs = obs)))
        else if ( ! is.null(biodb))
            testthat::test_that(msg, do.call(fct, list(biodb)))
        else if ( ! is.null(conn) && ! is.null(obs))
            testthat::test_that(msg, do.call(fct, list(conn = conn, obs = obs)))
        else if ( ! is.null(conn))
            testthat::test_that(msg, do.call(fct, list(conn)))
        else
            stop(paste0('Do not know how to call test function "', fname, '".'))
    }

    invisible(NULL)
}

# Get test log file descriptor {{{1
################################################################

#' @export
getTestLogFD <- function() {

    if ( ! exists('BIODB_TEST_LOG_FD')) {
        fp <- file.path(getwd(), 'biodb_test.log')
        assign('BIODB_TEST_LOG_FD', file(fp, open='w'), pos=.GlobalEnv)
    }

    return(BIODB_TEST_LOG_FD)
}
