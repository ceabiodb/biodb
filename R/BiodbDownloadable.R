# vi: fdm=marker ts=4 et cc=80

# BiodbDownloadable {{{1
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
    "Get the path where the downloaded containt is written."

    cch <- .self$getBiodb()$getCache()
    ext <- .self$getPropertyValue('dwnld.ext')
    path <- cch$getFilePath(.self$getCacheId(), subfolder='longterm',
                            name='download', ext=ext)

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
    "Returns TRUE if the database containt has already been downloaded."

    already.downloaded <- file.exists(.self$getDownloadPath())

    dwnlded <- (if (already.downloaded) 'already' else 'not yet')
    .self$debug('Database ', .self$getId(), ' has ', dwnlded,
                ' been downloaded.')

    return(already.downloaded)
},

# Is extracted {{{3
################################################################################

isExtracted=function() {
    "Returns TRUE of the downloaded database containt has been extracted."

    cch <- .self$getBiodb()$getCache()
    return(cch$markerExist(.self$getCacheId(), subfolder='shortterm',
                           name='extracted'))
},

# Download {{{3
################################################################################

download=function() {
    "Download the database containt locally."

    cch <- .self$getBiodb()$getCache()

    # Download
    cfg <- .self$getBiodb()$getConfig()
    if (cch$isWritable() && ! .self$isDownloaded()
        && (cfg$isEnabled('allow.huge.downloads') || .self$requiresDownload())
        && ! cfg$isEnabled('offline')) {

        .self$info("Downloading whole database of ", .self$getId(), ".")
        .self$.doDownload()
        .self$debug('Downloading of ', .self$getId(), ' completed.')
    }

    # Extract
    if (.self$isDownloaded() && ! .self$isExtracted()) {

        .self$info("Extract whole database of ", .self$getId(), ".")

        .self$.doExtractDownload()

        # Set marker
        cch$setMarker(.self$getCacheId(), subfolder='shortterm',
                      name='extracted')
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
