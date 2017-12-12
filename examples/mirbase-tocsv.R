# Create a Biodb instance
mybiodb <- biodb::Biodb$new()

# Get some entry IDs from Mirbase Mature.
entry.ids <- mybiodb$getFactory()$getConn('mirbase.mature')$getEntryIds(max.results = 10)

# Get entries
entries <- mybiodb$getFactory()$getEntry('mirbase.mature', id = entry.ids)

# Transform entries into a single data frame
df <- mybiodb$entriesToDataframe(entries)

# Export the data frame into a CSV file with R standard function
write.csv(df, file = 'mirbase-mature.csv')
