# Create a Biodb instance
mybiodb <- biodb::Biodb$new()

# Get connector to Massbank
massbank <- mybiodb$getFactory()$getConn('massbank')

# Search for MS spectra
massbank$searchMzTol(mz = 64, mz.tol  = 0.3, ms.level = 1, ms.mode = 'pos', max.results = 10)
