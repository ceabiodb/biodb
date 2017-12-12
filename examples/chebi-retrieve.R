# Create a Biodb instance
mybiodb <- biodb::Biodb$new()

# Request entries from ChEBI, using accession numbers
entries <- mybiodb$getFactory()$getEntry('chebi', id = c('2528', '17053', '15440'))

# Get the SMILES of those entries 
vapply(entries, function(e) e$getFieldValue('smiles'), FUN.VALUE = '')
