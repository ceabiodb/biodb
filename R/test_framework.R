#' A class for acknowledging messages during tests.
#'
#' This observer is used to call a testthat::expect_*() method each time a
#' message is received. This is used when running tests on Travis-CI, so Travis
#' does not stop tests because no change is detected in output.
#'
#' @examples
#' # To use the acknowledger, set ack=TRUE when creating the Biodb test
#' # instance:
#' biodb <- createBiodbTestInstance(ack=TRUE)
#'
#' # Terminate the BiodbMain instance
#' biodb$terminate()
#'
#' @import methods
#' @include BiodbObserver.R
#' @export BiodbTestMsgAck
#' @exportClass BiodbTestMsgAck
BiodbTestMsgAck <- methods::setRefClass('BiodbTestMsgAck',
    contains = 'BiodbObserver',
    fields = list(
        .last.index = 'numeric'
        ),

    methods=list(

initialize=function(...) {

    callSuper(...)

    .self$.last.index <- 0
},

notifyProgress=function(what, index, total) {
    # Override super class' method
    testthat::expect_type(what, 'character')
    testthat::expect_true(what != '') # --> expect_not_empty_str
    #testthat::expect_is(index, 'number') # --> expect_whole_number
    #testthat::expect_is(total, 'number')
    testthat::expect_true(index >= 0) # --> expect_positive
    testthat::expect_true(index <= total)
    return(invisible(NULL))
}
))

#' Set a test context.
#'
#' Define a context for tests using testthat framework.
#' In addition to calling `testthat::context()`.
#'
#' @param text The text to print as test context.
#' @return No value returned.
#'
#' @examples
#' # Define a context before running tests:
#' biodb::testContext("Test my database connector.")
#'
#' # Instantiate a BiodbMain instance for testing
#' biodb <- biodb::createBiodbTestInstance()
#'
#' # Terminate the instance
#' biodb$terminate()
#'
#' @export
testContext <- function(text) {

    # Set testthat context
    testthat::context(text)

    # Print banner in log file
    biodb::logInfo("")
    biodb::logInfo(paste(rep('*', 80), collapse=''))
    biodb::logInfo(paste("Test context", text, sep = " - "))
    biodb::logInfo(paste(rep('*', 80), collapse=''))
    biodb::logInfo("")

    return(invisible(NULL))
}

#' Run a test.
#'
#' Run a test function, using testthat framework.
#' In addition to calling `testthat::test_that()`.
#'
#' @param msg The test message.
#' @param fct The function to test.
#' @param biodb A valid BiodbMain instance to be passed to the test function.
#' @param conn A connector instance to be passed to the test function.
#' @param opt A set of options to pass to the test function.
#' @return No value returned.
#'
#' @examples
#' # Define a context before running tests:
#' biodb::testContext("Test my database connector.")
#'
#' # Instantiate a BiodbMain instance for testing
#' biodb <- biodb::createBiodbTestInstance()
#'
#' # Define a test function
#' my_test_function <- function(biodb) {
#'   # Do my tests...
#' }
#'
#' # Run test
#' biodb::testThat("My test works", my_test_function, biodb=biodb)
#'
#' # Terminate the instance
#' biodb$terminate()
#' @export
testThat  <- function(msg, fct, biodb=NULL, conn=NULL, opt=NULL) {

    # Get biodb instance
    if ( ! is.null(biodb) && ! methods::is(biodb, 'BiodbMain'))
        stop("`biodb` parameter must be a rightful biodb::BiodbMain instance.")
    if ( ! is.null(conn) && ! methods::is(conn, 'BiodbConn'))
        stop("`conn` parameter must be a rightful biodb::BiodbConn instance.")
    bdb <- if (is.null(conn)) biodb else conn$getBiodb()

    # Get function name
    if (methods::is(fct, 'function'))
        fname <- deparse(substitute(fct))
    else
        fname <- fct

    # Get list of test functions to run
    functions <- NULL
    if ( ! is.null(bdb) && bdb$getConfig()$isDefined('test.functions')) {
        functions <- bdb$getConfig()$get('test.functions')
    } else if ('BIODB_TEST_FUNCTIONS' %in% names(Sys.getenv())) {
        functions <- Sys.getenv()[['BIODB_TEST_FUNCTIONS']]
    }
    if ( ! is.null(functions)) # Convert to vector
        functions <- strsplit(functions, ',')[[1]]

    # Filter
    runFct <- if (is.null(functions)) TRUE else fname %in% functions

    if (runFct) {

        # Send message to logger
        biodb::logInfo('')
        biodb::logInfo(paste('Running test function ', fname, ' ("', msg, '").'))
        biodb::logInfo(paste(rep('-', 80), collapse=''))
        biodb::logInfo('')

        # Call test function
        params <- list()
        fctArgs <- formalArgs(fct)
        if ( ! is.null(fctArgs))
            for (p in fctArgs)
                params[[p]] <- if (p == 'db') conn else get(p)
        testthat::test_that(msg, do.call(fct, params))
    }

    invisible(NULL)
}

#' Creating a BiodbMain instance for tests.
#'
#' Creates a BiodbMain instance with options specially adapted for tests.
#' You can request the logging of all messages into a log file.
#' It is also possible to ask for the creation of a BiodbTestMsgAck observer,
#' which will receive all messages and emit a testthat test for each message.
#' This will allow the testthat output to not stall a long time while, for
#' example, downloading or extracting a database.
#' Do not forget to call `terminate()` on your instance at the end of your
#' tests.
#'
#' @param ack If set to TRUE, an instance of BiodbTestMsgAck will be attached to
#' the BiodbMain instance.
#' @return The created BiodbMain instance.
#'
#' @examples
#' # Instantiate a BiodbMain instance for testing
#' biodb <- biodb::createBiodbTestInstance()
#'
#' # Terminate the instance
#' biodb$terminate()
#'
#' @export
createBiodbTestInstance <- function(ack=FALSE) {

    # Create instance
    biodb <- BiodbMain$new(autoloadExtraPkgs=FALSE)

    # Add acknowledger
    if (ack) {
        ack <- BiodbTestMsgAck()
        biodb$addObservers(ack)
    }

    return(biodb)
}

#' List test reference entries.
#'
#' Lists the reference entries in the test folder for a specified connector.
#' The test reference files must be in `<pkg>/tests/testthat/res/` folder and
#' their names must match `entry-<database_name>-<entry_accession>.json` (e.g.:
#' `entry-comp.csv.file-1018.json`).
#'
#' @param conn.id A valid Biodb connector ID.
#' @param limit The maximum number of entries to retrieve.
#' @return A list of entry IDs.
#'
#' @examples
#' # List IDs of test reference entries:
#' biodb::listTestRefEntries('comp.csv.file')
#'
#' @export
listTestRefEntries <- function(conn.id, limit=0) {

    chk::chk_string(conn.id)
    chk::chk_whole_number(limit)
    chk::chk_gte(limit, 0)

    # List json files
    files <- Sys.glob(file.path(getwd(), '..', 'testthat', 'res',
                                paste('entry', conn.id, '*.json', sep='-')))
    if (limit > 0 && length(files) > limit)
        files <- files[seq_len(limit)]

    # Extract ids
    ids <- sub(paste('^.*/entry', conn.id, '(.+)\\.json$', sep='-'), '\\1',
               files, perl=TRUE)

    # Replace encoded special characters
    ids = vapply(ids, utils::URLdecode, FUN.VALUE='')

    return(ids)
}

loadTestRefEntry <- function(db, id) {

	# Replace forbidden characters
	id = utils::URLencode(id, reserved=TRUE)

	# Entry file
	file <- file.path(getwd(), '..', 'testthat', 'res',
                      paste('entry-', db, '-', id, '.json', sep=''))
	testthat::expect_true(file.exists(file),
                          info=paste0('Cannot find file "', file, '" for ', db,
                                      ' reference entry', id, '.'))

	# Load JSON
	json <- jsonlite::fromJSON(file)

	# Set NA values
	for (n in names(json))
		if (length(json[[n]]) == 1) {
			if (json[[n]] == 'NA_character_')
				json[[n]] <- NA_character_
		}

	return(json)
}

loadTestRefEntries <- function(db) {

	entries.desc <- NULL

	# List JSON files
	entry.json.files <- Sys.glob(file.path(getwd(), '..', 'testthat', 'res',
                                           paste('entry', db, '*.json',
                                                 sep='-')))

	# Loop on all JSON files
	for (f in entry.json.files) {

		# Load entry from JSON
		entry <- jsonlite::read_json(f)

		# Replace NULL values by NA
		entry <- lapply(entry, function(x) if (is.null(x)) NA else x)

		# Convert to data frame
		entry.df <- as.data.frame(entry, stringsAsFactors = FALSE)

		# Append entry to main data frame
		entries.desc <- plyr::rbind.fill(entries.desc, entry.df)
	}

	return(entries.desc)
}

#' Get the test output directory.
#'
#' Returns the path to the test output directory. The function creates this also
#' this directory if it does not exist.
#'
#' @return The path to the test output directory, as a character value.
#'
#' @examples
#' # Get the test output directory:
#' biodb::getTestOutputDir()
#'
#' @export
getTestOutputDir <- function() {

    p <- file.path(getwd(), 'output')
    if ( ! dir.exists(p))
        dir.create(p)

    return(p)
}
