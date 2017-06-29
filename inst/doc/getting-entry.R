## ------------------------------------------------------------------------
# Create a Biodb instance
mybiodb <- biodb::Biodb$new()

# Request the entry 2528 in ChEBI
entry <- mybiodb$getFactory()$getEntry('chebi', id = '2528')

## ------------------------------------------------------------------------
cat(entry$getFieldValue('accession'), "\n", sep = '')

