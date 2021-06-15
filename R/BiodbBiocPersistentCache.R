#' A persistent cache implementation that uses BiocCache package.
#'
#'
#' @seealso \code{\link{BiodbPersistentCache}},
#' \code{\link{BiodbBiocPersistentCache}}.
#'
#' @include BiodbPersistentCache.R
#' @export BiodbBiocPersistentCache
#' @exportClass BiodbBiocPersistentCache
BiodbBiocPersistentCache <- methods::setRefClass('BiodbBiocPersistentCache',
    contains='BiodbPersistentCache',
    methods=list(

# TODO Make one BiocFileCache object for each cache.id 
# path <- tools::R_user_dir('biodb', which="cache")
# path <- file.path(tools::R_user_dir('biodb', which="cache"), cache.id)
# bfc <- BiocFileCache::BiocFileCache(path, ask=FALSE)
))
