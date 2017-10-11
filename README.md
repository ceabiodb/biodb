Biodb
=====

[![Build Status](https://travis-ci.org/pkrog/biodb.svg?branch=master)](https://travis-ci.org/pkrog/biodb)

An R package for connecting to chemical and biological databases.

With *biodb* you can:

 * Access entries by accession number and let *biodb* download them for you.
 * Take advantage of the cache system, that saves the results of all sent requests for you. If you send again the same request, the cached result will be used instead of contacting the database. The cache system can be disabled.
 * Download whole databases and access entries by accession number locally.
 * Rely on *biodb* to access correctly the database, respecting the published access policy (i.e.: not sending too much requests). *biodb* uses a special class for scheduling requests on each database.
 * Switch from one database to another easily (providing they offer the same type of information), not changing a line in your code. This is because entries are populated with values found from the database, using always the same keys.
 * Search for MS spectra by peaks in Massbank, Peakforest and in-house database.
 * Search for MSMS spectra.
 * Export any database into a CSV file.

## License

This software is licensed under the GNU AGPLv3 license.

## Examples

In this section, you can find some of the possibilities offered by the *biodb* package.
More examples, explained and detailed, can be found in the package's vignettes.

### Retrieve some entries from a database

```r
# Create a Biodb instance
mybiodb <- biodb::Biodb$new()

# Request entries from ChEBI, using accession numbers
entries <- mybiodb$getFactory()$getEntry('chebi', id = c('2528', '17053', '15440'))

# Get the SMILES of those entries 
smiles <- vapply(entries, function(e) e$getFieldValue('smiles'), FUN.VALUE = '')
```

### Exporting a database into a CSV file

```r
# Create a Biodb instance
mybiodb <- biodb::Biodb$new()

# Get all entry IDs of Mirbase Mature.
entry.ids <- mybiodb$getFactory()$getConn('mirbase.mature')$getEntryIds()

# Get all Mirbase entries
entries <- mybiodb$getFactory()$getEntry('mirbase.mature', id = entry.ids)

# Transform all entries into a single data frame
df <- mybiodb$entriesToDataframe(entries)

# Export the data frame into a CSV file with R standard function
write.csv(df, file = 'mirbase-mature.csv')
```

### Search for MS spectra

```r
# Create a Biodb instance
mybiodb <- biodb::Biodb$new()

# Get connector to Massbank
massbank <- mybiodb$getFactory()$getConn('massbank.jp')

# Search for MS spectra
spectra.ids <- massbank$searchMzTol(mz = 64, tol  = 0.3, ms.level = 1, max.results = 10)
```

### Search for MSMS spectra

```r
# Create a Biodb instance
mybiodb <- biodb::Biodb$new()

# Get connector to Massbank
massbank <- mybiodb$getFactory()$getConn('massbank.jp')

spectrum <- data.frame(mz = c(64), rel.int = c(100))

# Search for MS spectra
spectra.ids <- massbank$msmsSearch(spectrum, precursor.mz = 100, mz.tol = 0.3)
```

## Installation

### Install from GitHub

Using package `devtools` you can install Biodb directly from GitHub:
```r
devtools::install_github('pkrog/biodb')
```

### Install from local repository

First, you need to clone the GitHub repos:
```bash
git clone https://github.com/pkrog/biodb.git
```

Then you install *biodb* using the `devtools` package:
```r
devtools::install_local('/your/path/to/biodb')
```

## Access documentation

Once in R, you can get documentation about main classes with the standard help function:
```r
?biodb::Biodb
```
Some of the classes you can get help about are: Biodb, BiodbFactory, BiodbConfig, BiodbCache, BiodbDbsInfo, BiodbEntryFields, BiodbObserver, BiodbConn, BiodbEntry, MassdbConn, RemotedbConn.

Some vignettes are also available. To get a list of them run:
```r
vignette(package = 'biodb')
```

To open a vignette in a browser, use its name:
```r
vignette('init', package = 'biodb')
```

## Contributing

If you wish to contribute to the *biodb* package, you first need to create an account under GitHub. You can then either to ask to become a contributor or fork the project and submit a merge request.

Debugging, enhancement or creation of a database connector or an entry parser are of course most welcome.

### Running tests

Under UNIX and UNIX like systems (macOS, Linux, ...) you can run `make test` to run the tests. You will need to have R accessible from command line, and also to have installed the R package `testthat`.

The plain command `make test` will run the offline tests, which uses cache files recorded inside this repository under `tests/res/offline-cache`. All databases will be tested.

If you wish to test only some databases, you can specify them inside the environment variable `DATABASES`:
```bash
DATABASES=massbank.jp,chebi,mirbase.mature make test
```

If you want to run online tests, use the environment variable `MODES` to specify it:
```bash
MODES=online make test
```

The value `quick.online` for `MODES` turns off download of whole databases if they have already been downloaded and are stored inside the cache system.
The value `all` for `MODES` run tests in all modes: `online`, `quick.online` and `offline`.

The two environment variables can be combined together.
