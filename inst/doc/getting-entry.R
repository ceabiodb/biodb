## ------------------------------------------------------------------------
# Create a Biodb instance
mybiodb <- biodb::Biodb$new()

## ------------------------------------------------------------------------
print(mybiodb$getDbsInfo()$getIds())

## ------------------------------------------------------------------------
# Request the entry 2528 in ChEBI
entry <- mybiodb$getFactory()$getEntry('chebi', id = '2528')

## ------------------------------------------------------------------------
print(entry$getFieldValue('accession'))

