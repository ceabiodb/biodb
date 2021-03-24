getPkgName <- function(pkgRoot) {
    name <- basename(pkgRoot)
    chk::chk_match(name, regexp="^biodb[A-Z][A-Za-z0-9]+$")
    return(name)
}
