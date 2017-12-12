# Create a Biodb instance
mybiodb <- biodb::Biodb$new()

# Request entries from ChEBI, using accession numbers
chebi.entries <- mybiodb$getFactory()$getEntry('chebi', id = c('2528', '17053'))

# Request entries from Uniprot
uniprot.entries <- mybiodb$getFactory()$getEntry('uniprot', id = c('P02461', 'P08123', 'Q31611'))

# Request entries from LIPID MAPS Structure
lipidmaps.entries <- mybiodb$getFactory()$getEntry('lipidmaps.structure', id = c('LMFA08040013'))

# Get a data frame of all these entries
mybiodb$entriesToDataframe(c(chebi.entries, uniprot.entries, lipidmaps.entries), fields = c('accession', 'name', 'mass'))
