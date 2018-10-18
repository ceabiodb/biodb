# Create a Biodb instance
mybiodb <- biodb::Biodb$new()

# Get connector to Massbank
massbank <- mybiodb$getFactory()$getConn('massbank')

# Create spectrum to search for
spectrum <- data.frame(mz = c(156.0124, 252.0446), rel.int = c(999, 158))

# Search for matching MS spectra
massbank$msmsSearch(spectrum, precursor.mz = 252.0448, mz.tol = 0.1, mz.tol.unit = 'plain', ms.mode = 'neg')
