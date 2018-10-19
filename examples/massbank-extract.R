# Create a Biodb instance
mybiodb <- biodb::Biodb$new()

# Get Massbank connector
mb <- mybiodb$getFactory()$createConn('massbank')

# Search entries containing one peak
ids <- mb$getEntryIds(max.results = 3, ms.level = 1)
entries <- mybiodb$getFactory()$getEntry('massbank', ids)

# Convert to data frame
mybiodb$entriesToDataframe(entries, only.atomic = FALSE, fields = c('massbank.id', 'name', 'formula', 'peaks'))
