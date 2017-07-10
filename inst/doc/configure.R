## ------------------------------------------------------------------------
mybiodb <- biodb::Biodb$new()

## ------------------------------------------------------------------------
config <- mybiodb$getConfig()

## ------------------------------------------------------------------------
print(config$getKeys())

## ------------------------------------------------------------------------
print(config$getDescription('cache.directory'))

## ------------------------------------------------------------------------
print(config$get('cache.directory'))

## ------------------------------------------------------------------------
config$set('cache.directory', '~/my.biodb.cache')

## ------------------------------------------------------------------------
print(config$get('cache.directory'))

## ------------------------------------------------------------------------
print(config$getDefaultValue('cache.directory'))

## ------------------------------------------------------------------------
print(config$get('offline'))
config$enable('offline')    # set to TRUE
config$disable('offline')   # set to FALSE
print(if (config$isEnabled('offline')) 'Mode offline is ON.' else 'Mode offline is OFF.')

