## ------------------------------------------------------------------------
mybiodb <- biodb::Biodb$new()
mybiodb$getConfig()$set('chemspider.token', '01234567-89ab-cdef-0123-456789abcdef')

## ------------------------------------------------------------------------
mybiodb <- biodb::Biodb$new()
dbsinfo <- mybiodb$getDbsInfo()
dbsinfo$get('chemspider')$setToken('01234567-89ab-cdef-0123-456789abcdef')

## ------------------------------------------------------------------------
mybiodb <- biodb::Biodb$new()
factory <- mybiodb$getFactory()
conn <- factory$createConn('chemspider', token = '01234567-89ab-cdef-0123-456789abcdef')

