#' A biodb custom persistent cache implementation.
#'
#'
#' Inside the cache folder, one folder is created for each cache ID (each remote
#' database has one single cache ID, always the same ,except if you change its
#' URL). In each of theses folders are stored the cache files for this database.
#'
#' @seealso \code{\link{BiodbPersistentCache}},
#' \code{\link{BiodbBiocPersistentCache}}.
#'
#' @include BiodbPersistentCache.R
#' @export BiodbCustomPersistentCache
#' @exportClass BiodbCustomPersistentCache
BiodbCustomPersistentCache <- methods::setRefClass('BiodbCustomPersistentCache',
    contains='BiodbPersistentCache',
    methods=list(
))
