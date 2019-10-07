# vi: fdm=marker ts=4 et cc=80 tw=80

# BiodbDownloadable {{{1
################################################################################

# Declaration {{{2
################################################################################

#' An abstract class (more like an interface) to model a remote database that
#' can be downloaded locally.
#'
#' The class must be inherited by any remote database class that allows download
#' of its whole content. The hidden/private method \code{.doDownload()} must be
#' implemented by the database class.
#'
#' @seealso \code{\link{BiodbRemotedbConn}}.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Use a downloadable database
#' mirbase <- mybiodb$getFactory()$getConn('mirbase.mature')
#'
#' # Check if database has been downloaded
#' mirbase$isDownloaded()
#'
#' # Download the whole database content
#' \dontrun{
#' mirbase$download()
#' }
#' # Not though that you do not need to call this method explicitly. This is
#' # will be done automatically by Biodb if needed.
#'
#' # Terminate instance.
#' mybiodb$terminate()
#' mybiodb <- NULL
#'
#' @import methods
#' @include BiodbObject.R
#' @export BiodbDownloadable
#' @exportClass BiodbDownloadable
BiodbDownloadable <- methods::setRefClass("BiodbDownloadable",
    contains='BiodbObject',

# Public methods {{{2
################################################################################

methods=list(

# Initialize {{{3
################################################################################

initialize=function(...) {
    callSuper(...)
    .self$.abstractClass('BiodbDownloadable')
},

# Get download path {{{3
################################################################################

getDownloadPath=function() {
    ":\n\nGets the path where the downloaded content is written.
    \nReturned value: The path where the downloaded database is written.
    "

    cch <- .self$getBiodb()$getCache()
    ext <- .self$getPropertyValue('dwnld.ext')
    path <- cch$getFilePath(.self$getCacheId(), name='download', ext=ext)

    .self$debug('Download path of ', .self$getId(), ' is "', path, '".')

    return(path)
},

# Requires download {{{3
################################################################################

requiresDownload=function() {
    return(FALSE)
},

# Is downloaded {{{3
################################################################################

isDownloaded=function() {
    ":\n\nTests if the database has been downloaded.
    \nReturned value: TRUE if the database content has already been downloaded.
    "

    cch <- .self$getBiodb()$getCache()
    dwnlded  <- cch$markerExist(.self$getCacheId(),
                    name='downloaded')

    s <- (if (dwnlded) 'already' else 'not yet')
    .self$debug('Database ', .self$getId(), ' has ', s,
                ' been downloaded.')

    return(dwnlded)
},

# Is extracted {{{3
################################################################################

isExtracted=function() {
    ":\n\nTests if the downloaded database has been extracted (in case the
    database needs extraction).
    \nReturned value: TRUE if the downloaded database content has been
    extracted, FALSE otherwise.
    "

    cch <- .self$getBiodb()$getCache()
    return(cch$markerExist(.self$getCacheId(),
                           name='extracted'))
},

# Download {{{3
################################################################################

download=function() {
    ":\n\nDownloads the database content locally.
    \nReturned value: None.
    "

    cch <- .self$getBiodb()$getCache()

    # Download
    cfg <- .self$getBiodb()$getConfig()
    if (cch$isWritable() && ! .self$isDownloaded()
        && (cfg$isEnabled('allow.huge.downloads') || .self$requiresDownload())
        && ! cfg$isEnabled('offline')) {

        .self$info("Downloading whole database of ", .self$getId(), ".")
        .self$.doDownload()
        .self$debug('Downloading of ', .self$getId(), ' completed.')

        # Set marker
        cch$setMarker(.self$getCacheId(), name='downloaded')
    }

    # Extract
    if (.self$isDownloaded() && ! .self$isExtracted()) {

        .self$info("Extract whole database of ", .self$getId(), ".")

        .self$.doExtractDownload()

        # Set marker
        cch$setMarker(.self$getCacheId(), name='extracted')
    }
},

# Private methods {{{2
################################################################################

# Do download {{{3
################################################################################

.doDownload=function() {
    .self$.abstractMethod()
}

))
