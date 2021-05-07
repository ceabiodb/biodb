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
#' # Creating a connector class for a downloadable database:
#' FooDownloadableDb <- methods::setRefClass('FooDownloadableDb',
#'     contains=c('BiodbRemotedbConn', 'BiodbDownloadable'),
#'
#' methods=list(
#'
#'    # ... other methods ...
#'
#'    # BiodbDownloadable methods
#'    .doDownload=function() {
#'        # Download the database
#'    },
#'
#'    .doExtractDownload=function() {
#'        # Extract data from the downloaded file(s)
#'    }
#' ))
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

    logDebug0('Download path of ', .self$getId(), ' is "', path, '".')

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
    logDebug0('Database ', .self$getId(), ' has ', s, ' been downloaded.')

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

        logInfo0("Downloading whole database of ", .self$getId(), ".")
        .self$.doDownload()
        if ( ! file.exists(.self$getDownloadPath()))
            error("File %s does not exists. Downloading went wrong.",
                  .self$getDownloadPath())
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
