#' A class for acknowledging messages during tests.
#'
#' This observer is used to call a testthat::expect_*() method each time a
#' message is received. This is used when running tests on Travis-CI, so Travis
#' does not stop tests because no change is detected in output.
#'
#' @examples
#' # To use the acknowledger, set ack=TRUE when creating the Biodb test
#' # instance:
#' biodb <- biodb::createBiodbTestInstance(ack=TRUE)
#'
#' # Terminate the BiodbMain instance
#' biodb$terminate()
#'
#' @import R6
#' @import methods
BiodbTestMsgAck <- R6::R6Class('BiodbTestMsgAck',

public=list(

#' @description
#' New instance initializer.
#' @return Nothing.
initialize=function() {

    private$last.index <- 0
    
    return(invisible(NULL))
},

#' @description
#' Call back method used to get progress advancement of a long process.
#' @param what The reason as a character value.
#' @param index The index number representing the progress.
#' @param total The total number to reach for completing the process.
#' @return Nothing.
notifyProgress=function(what, index, total) {

    testthat::expect_true(index >=0 && (is.na(total) || index <= total))

    return(invisible(NULL))
}
),

private=list(
    last.index=NULL
))
