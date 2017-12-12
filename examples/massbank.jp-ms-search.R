# Create a Biodb instance
mybiodb <- biodb::Biodb$new()

# Get connector to Massbank
massbank <- mybiodb$getFactory()$getConn('massbank.jp')

# Search for MS spectra
massbank$searchMzTol(mz = 64, mz.tol  = 0.3, ms.level = 1, max.results = 10)
