ChebiExConn <- methods::setRefClass("ChebiExConn",
    contains=c("BiodbRemotedbConn", "BiodbCompounddbConn"),

methods=list(

initialize=function(...) {
    callSuper(...)
},

getEntryPageUrl=function(id) {
    # Overrides super class' method

    url <- c(.self$getPropValSlot('urls', 'base.url'), 'searchId.do')

    urls <- vapply(id, function(x) BiodbUrl(url=url,
                                            params=list(chebiId=x))$toString(),
                   FUN.VALUE='')

    return(urls)
},

getEntryImageUrl=function(id) {
    # Overrides super class' method

    url <- c(.self$getPropValSlot('urls', 'base.url'), 'displayImage.do')

    urls <- vapply(id,
                   function(x) BiodbUrl(url=url,
                                        params=list(defaultImage='true',
                                          imageIndex=0, chebiId=x,
                                          dimensions=400))$toString(),
                  FUN.VALUE='')

    return(urls)
},

searchCompound=function(name=NULL, mass=NULL, mass.field=NULL, mass.tol=0.01,
                        mass.tol.unit='plain', max.results=NA_integer_) {
    # Overrides super class' method.

    return(NULL)
},

.doGetEntryContentRequest=function(id, concatenate=TRUE) {

    url <- c(.self$getPropValSlot('urls', 'ws.url'), 'test',
             'getCompleteEntity')

    urls <- vapply(id, function(x) BiodbUrl(url=url,
                                            params=list(chebiId=x))$toString(),
                   FUN.VALUE='')

    return(urls)
},

.doGetEntryIds=function(max.results=NA_integer_) {
    return(NULL)
}
))
