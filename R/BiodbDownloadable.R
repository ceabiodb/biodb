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
#' \dontrun{
#' conn <- mybiodb$getFactory()$getConn('my_database')
#' }
#'
#' # Check if database has been downloaded
#' \dontrun{
#' conn$isDownloaded()
#' }
#'
#' # Download the whole database content
#' \dontrun{
#' conn$download()
#' }
#' # Note though that you do not need to call this method explicitly. This
#' # will be done automatically by biodb if needed.
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

methods=list(

initialize=function(...) {
    callSuper(...)
    .self$.abstractClass('BiodbDownloadable')
},

getDownloadPath=function() {
    ":\n\nGets the path where the downloaded content is written.
    \nReturned value: The path where the downloaded database is written.
    "

    cch <- .self$getBiodb()$getPersistentCache()
    ext <- .self$getPropertyValue('dwnld.ext')
    path <- cch$getFilePath(.self$getCacheId(), name='download', ext=ext)

    logDebug('Download path of ', .self$getId(), ' is "', path, '".',
             fmt='paste0')

    return(path)
},

requiresDownload=function() {
    return(FALSE)
},

isDownloaded=function() {
    ":\n\nTests if the database has been downloaded.
    \nReturned value: TRUE if the database content has already been downloaded.
    "

    cch <- .self$getBiodb()$getPersistentCache()
    dwnlded  <- cch$markerExist(.self$getCacheId(),
                    name='downloaded')

    s <- (if (dwnlded) 'already' else 'not yet')
    logDebug('Database ', .self$getId(), ' has ', s, ' been downloaded.',
             fmt='paste0')

    return(dwnlded)
},

isExtracted=function() {
    ":\n\nTests if the downloaded database has been extracted (in case the
    database needs extraction).
    \nReturned value: TRUE if the downloaded database content has been
    extracted, FALSE otherwise.
    "

    cch <- .self$getBiodb()$getPersistentCache()
    return(cch$markerExist(.self$getCacheId(),
                           name='extracted'))
},

download=function() {
    ":\n\nDownloads the database content locally.
    \nReturned value: None.
    "

    cch <- .self$getBiodb()$getPersistentCache()

    # Download
    cfg <- .self$getBiodb()$getConfig()
    if (cch$isWritable(.self) && ! .self$isDownloaded()
        && (cfg$isEnabled('allow.huge.downloads') || .self$requiresDownload())
        && ! cfg$isEnabled('offline')) {

        logInfo("Downloading whole database of ", .self$getId(), ".",
                fmt='paste0')
        .self$.doDownload()
        logDebug0('Downloading of ', .self$getId(), ' completed.')

        # Set marker
        cch$setMarker(.self$getCacheId(), name='downloaded')
    }

    # Extract
    if (.self$isDownloaded() && ! .self$isExtracted()) {

        logInfo0("Extract whole database of ", .self$getId(), ".")

        .self$.doExtractDownload()

        # Set marker
        cch$setMarker(.self$getCacheId(), name='extracted')
    }
},

.doDownload=function() {
    .self$.abstractMethod()
},

.doExtractDownload=function() {
    .self$.abstractMethod()
}

))
