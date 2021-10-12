#' List test reference entries.
#'
#' DEPRECATED. Use TestRefEntries class instead.
#'
#' Lists the reference entries in the test folder for a specified connector.
#' The test reference files must be in `<pkg>/inst/testref/` folder and
#' their names must match `entry-<database_name>-<entry_accession>.json` (e.g.:
#' `entry-comp.csv.file-1018.json`).
#'
#' @param conn.id A valid Biodb connector ID.
#' @param limit   The maximum number of entries to retrieve.
#' @param pkgName The name of the 
#' @return A list of entry IDs.
#'
#' @examples
#' # List IDs of test reference entries:
#' biodb::listTestRefEntries('comp.csv.file', pkgName='biodb')
#'
#' @export
listTestRefEntries <- function(conn.id, pkgName, limit=0) {
    lifecycle::deprecate_warn('1.1.0', "listTestRefEntries()",
        "TestRefEntries::getAllIds()")
    return(TestRefEntries$new(conn.id,
        pkgName=pkgName)$getAllIds(limit=limit))
}

loadTestRefEntries <- function(db, pkgName) {
    lifecycle::deprecate_warn('1.1.0', "loadTestRefEntries()",
        "TestRefEntries::getAllRefEntriesDf()")
    return(TestRefEntries$new(db,
        pkgName=pkgName)$getAllRefEntriesDf())
}

loadTestRefEntry <- function(db, id, pkgName) {
    lifecycle::deprecate_warn('1.1.0', "loadTestRefEntry()",
        "TestRefEntries::getRefEntry()")
    return(TestRefEntries$new(db.class=db, pkgName=pkgName)$getRefEntry(id))
}

getTestRefFolder <- function(pkgName) {
    lifecycle::deprecate_warn('1.1.0', "getTestRefFolder()",
        "TestRefEntries::getFolder()")
    return(TestRefEntries$new(db.class='some.fake.class',
        pkgName=pkgName)$getFolder())
}
