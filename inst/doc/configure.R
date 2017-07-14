## ------------------------------------------------------------------------
mybiodb <- biodb::Biodb$new()

## ------------------------------------------------------------------------
config <- mybiodb$getConfig()

## ------------------------------------------------------------------------
config$getKeys()

## ------------------------------------------------------------------------
config$getDescription('cache.directory')

## ------------------------------------------------------------------------
config$get('cache.directory')

## ------------------------------------------------------------------------
config$set('cache.directory', '~/my.biodb.cache')

## ------------------------------------------------------------------------
config$get('cache.directory')

## ------------------------------------------------------------------------
config$getDefaultValue('cache.directory')

## ------------------------------------------------------------------------
config$get('offline')
config$enable('offline')    # set to TRUE
config$disable('offline')   # set to FALSE
if (config$isEnabled('offline')) 'Mode offline is ON.' else 'Mode offline is OFF.'

