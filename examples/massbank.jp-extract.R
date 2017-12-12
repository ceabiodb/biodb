# Create a Biodb instance
mybiodb <- biodb::Biodb$new()

# Get Massbank connector
mb <- mybiodb$getFactory()$createConn('massbank.jp')

# Search entries containing one peak
ids <- mb$searchMzTol(mz = 132, mz.tol = 0.1, max.results = 3, ms.mode = 'neg', ms.level = 2)
entries <- mybiodb$getFactory()$getEntry('massbank.jp', ids)

# Convert to data frame
mybiodb$entriesToDataframe(entries, only.atomic = FALSE, fields = c('massbank.jp.id', 'name', 'formula', 'peaks'))
