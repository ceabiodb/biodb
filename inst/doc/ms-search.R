## ------------------------------------------------------------------------
mybiodb <- biodb::Biodb$new()

## ------------------------------------------------------------------------
conn <- mybiodb$getFactory()$createConn('massbank.jp')

## ------------------------------------------------------------------------
conn$getMzValues(max.results = 10)

## ------------------------------------------------------------------------
conn$getMzValues(max.results = 10, ms.level = 2, precursor = TRUE, ms.mode = 'pos')

## ------------------------------------------------------------------------
conn$searchMzRange(mz.min = 54, mz.max = 54.1, max.results = 5)

## ------------------------------------------------------------------------
conn$searchMzTol(mz = 54, mz.tol = 0.1, mz.tol.unit = 'plain', max.results = 5)

## ------------------------------------------------------------------------
# Define spectrum to match:
spectrum <- data.frame(mz = c(100.100, 83.100), rel.int = c(100, 10))

# Search for match:
conn$msmsSearch(spectrum, precursor.mz = 100, mz.tol = 0.3)

