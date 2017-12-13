# Create a table of IDs
table <- data.frame(chebi = c('2528', '17053', '37600'), kegg = c('C08811', 'C00046', 'C08259'))

# Create a Biodb instance
mybiodb <- biodb::Biodb$new()

# Request entries from ChEBI
entries <- mybiodb$getFactory()$getEntry('chebi', id = table[['chebi']])

# Get a data frame of all these entries
df <- mybiodb$entriesToDataframe(entries, fields = c('chebi.id', 'kegg.compound.id'))

# Search for rows containing mistake in KEGG ID
table[table[['kegg']] != df[['kegg.compound.id']], ]
