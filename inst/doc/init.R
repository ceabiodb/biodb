## ------------------------------------------------------------------------
mybiodb <- biodb::Biodb()

## ------------------------------------------------------------------------
mybiodb <- biodb::Biodb$new(logger = FALSE)

## ------------------------------------------------------------------------
mybiodb <- biodb::Biodb$new(logger = FALSE, observers = biodb::BiodbLogger$new(file = 'mybiodb.log'))

## ------------------------------------------------------------------------
mybiodb <- biodb::Biodb$new(logger = FALSE, observers = list(biodb::BiodbLogger$new(), biodb::BiodbLogger$new(file = 'mybiodb2.log')))

## ------------------------------------------------------------------------
mybiodb$addObservers( list(biodb::BiodbLogger$new(), biodb::BiodbLogger$new(file = 'mybiodb3.log')))

## ------------------------------------------------------------------------
mybiodb$getObservers()

## ------------------------------------------------------------------------
factory <- mybiodb$getFactory()

## ------------------------------------------------------------------------
config <- mybiodb$getConfig()

## ------------------------------------------------------------------------
cache <- mybiodb$getCache()

## ------------------------------------------------------------------------
dbsinfo <- mybiodb$getDbsInfo()

## ------------------------------------------------------------------------
entry.fields <- mybiodb$getEntryFields()

