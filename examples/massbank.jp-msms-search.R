# Create a Biodb instance
mybiodb <- biodb::Biodb$new()

# Get connector to Massbank
massbank <- mybiodb$getFactory()$getConn('massbank.jp')

spectrum <- data.frame(mz = c(64), rel.int = c(100))

# Search for MS spectra
massbank$msmsSearch(spectrum, precursor.mz = 100, mz.tol = 0.3)
