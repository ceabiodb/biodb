# Create a Biodb instance
mybiodb <- biodb::Biodb$new()

# Get KEGG Compound connector
conn <- mybiodb$getFactory()$createConn('kegg.compound')

# Search for compounds of a certain mass
ids <- conn$searchCompound(mass = 64, mass.tol = 2.0, max.results = 10)
entries <- mybiodb$getFactory()$getEntry('kegg.compound', ids)

# Convert the list of entries into a data frame
mybiodb$entriesToDataframe(entries, fields = c('accession', 'mass', 'formula'))
