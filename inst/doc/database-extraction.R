## ------------------------------------------------------------------------
mybiodb <- biodb::Biodb()

## ------------------------------------------------------------------------
lipids <- mybiodb$getFactory()$getConn('lipidmaps.structure')

## ------------------------------------------------------------------------
entry.ids <- lipids$getEntryIds()

## ------------------------------------------------------------------------
entries <- mybiodb$getFactory()$getEntry('lipidmaps.structure', id = entry.ids[1:2])

## ------------------------------------------------------------------------
df <- mybiodb$entriesToDataframe(entries)
print(df)

## ------------------------------------------------------------------------
write.csv(df, file = 'lipidmaps-structure.csv')

## ------------------------------------------------------------------------
massbank <- mybiodb$getFactory()$getConn('massbank.jp')

## ------------------------------------------------------------------------
entry.ids <- massbank$searchMzTol(64, mz.tol = 0.3, max.results = 2)

## ------------------------------------------------------------------------
mybiodb$getConfig()$disable('allow.huge.downloads')

## ------------------------------------------------------------------------
entries <- mybiodb$getFactory()$getEntry('massbank.jp', id = entry.ids)

## ------------------------------------------------------------------------
df <- mybiodb$entriesToDataframe(entries, only.atomic = FALSE)
print(df)

## ------------------------------------------------------------------------
write.csv(df, file = 'massbank.csv')

