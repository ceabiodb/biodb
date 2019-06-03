# vi: fdm=marker ts=4 et cc=80

# Class declaration {{{1
################################################################################

NcbiPubchemSubstConn <- methods::setRefClass("NcbiPubchemSubstConn", contains="NcbiPubchemConn")

# Initialize {{{1
################################################################################

NcbiPubchemSubstConn$methods( initialize=function(...) {
    callSuper(db.name='substance', id.xmltag='PC-ID_id', entry.xmltag='PC-Substance', id.urlfield='sid', entrez.name='pcsubstance', ...)
})

