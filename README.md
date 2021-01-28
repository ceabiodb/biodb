# biodb package

[![Build Status](https://travis-ci.org/pkrog/biodb.svg?branch=master)](https://travis-ci.org/pkrog/biodb)
[![Codecov test coverage](https://codecov.io/gh/pkrog/biodb/branch/master/graph/badge.svg)](https://codecov.io/gh/pkrog/biodb?branch=master)

An R package for connecting to chemical and biological databases.

## Introduction

*biodb* is a framework for developing database connectors. It is delivered with some non-remote connectors (for CSV file or SQLite db), but the main interest of the package is to ease development of your own connectors. Some connectors are already available in other packages (e.g.: biodbChebi, biodbHmdb, biodbKegg, biodbUniprot) on GitHub or Bioconductor.
For now, the targeted databases are the ones that store molecules, proteins, lipids and MS spectra. However other type of databases (NMR database for instance) could also be targeted.

With *biodb* you can:

 * Define your own database connector.
 * Access entries by accession number and let *biodb* download them for you.
 * Take advantage of the cache system, that saves the results of all sent requests for you. If you send again the same request, the cached result will be used instead of contacting the database. The cache system can be disabled.
 * Download locally a downloadable database and access entries by accession number locally.
 * Rely on *biodb* to access correctly the database, respecting the published access policy (i.e.: not sending too much requests). *biodb* uses a special class for scheduling requests on each database.
 * Switch from one database to another easily (providing they offer the same type of information), not changing a line in your code. This is because entries are populated with values found from the database, using always the same keys.
 * Search for MS and MSMS spectra by peaks in Mass spectra databases.
 * Export any database into a CSV file or record it into an SQLite file.

## Installation

The package is currently in submission to Bioconductor.

In the meantime you can install the latest stable version with:
```r
install.packages('devtools')
devtools::install_github('pkrog/biodb', dependencies=TRUE)
```

Alongside *biodb* you can install the following R extension packages that use  *biodb* for implementing connectors to online databases:

 * [biodbChebi](https://github.com/pkrog/biodbChebi) for accessing the [ChEBI](https://www.ebi.ac.uk/chebi/) database.
 * [biodbHmdb](https://github.com/pkrog/biodbHmdb) for accessing the [HMDB](http://www.hmdb.ca/) database.
 * [biodbKegg](https://github.com/pkrog/biodbKegg) for accessing the [KEGG](https://www.kegg.jp/) databases.
 * [biodbUniprot](https://github.com/pkrog/biodbUniprot) for accessing the [UniProt](https://www.uniprot.org/) database.

Installation of one of those extension packages can be done with the following command (replace 'biodbKegg' with the name of the wanted package):
```r
devtools::install_github('pkrog/biodbKegg', dependencies=TRUE)
```

### Installation with Bioconda

**biodb** is part of [Bioconda](https://github.com/orgs/bioconda/dashboard), so you can install it using Conda. This means also that it is possible to install it automatically in Galaxy, for a tool, if the Conda system is enabled.

## Databases and fields accessible with biodb

The *biodb* package contains the following in-house database connectors:

 * Compound CSV File (an in-house database stored inside a CSV file).
 * Mass CSV File (an in-house database stored inside a CSV file).
 * Mass SQLite (an in-house database stored inside an SQLite file).

Here are some of the fields accessible through the retrieved entries (more fields are defined in extension packages):

 * Chemical formula.
 * InChI.
 * InChI Key.
 * SMILES.
 * Common names and IUPAC names.
 * Charge.
 * Average mass.
 * Monoisotopic mass.
 * Molecular mass.
 * MS device.
 * MS Level.
 * MS mode.
 * MS precursor M/Z.
 * MS precursor annotation.
 * Peaks' M/Z values.
 * Peaks' intensities.
 * Peaks' relative intensities.
 * Attributions of peaks.
 * Compositions of peaks.
 * Peak table.
 * Chromatographic column name.
 * Chromatographic column length.
 * Chromatographic column diameter.
 * Chromatographic solvent.
 * Chromatographic retention time.
 * Chromatographic retention time unit.

## Examples

### Getting entries from a remote database

Here is an example on how to retrieve entries from ChEBI database and get a data frames of them (you must first install both *biodb* and *biodbChebi* packages):
```r
bdb <- biodb::Biodb()
chebi <- bdb$getFactory()$createConn('chebi')
entries <- chebi$getEntry(c('2528', '7799', '15440'))
bdb$entriesToDataframe(entries)
```

### Searching for a compound

All compound databases (ChEBI, Compound CSV File, KEGG Compound, ...) can be searched for compounds using the same function. Once you have your connector instance, you just have to call `searchCompound()` on it:
```r
myconn$searchCompound(name='phosphate')
```
The function will return a character vector containing all identifiers of matching entries.

It is also possible to search by mass, choosing the mass field you want (if this mass particular field is handled by the database):
```r
myconn$searchCompound(mass=230.02, mass.field='monoisotopic.mass', mass.tol=0.01)
```

Searching by both name and mass is also possible.
```r
myconn$searchCompound(name='phosphate', mass=230.02, mass.field='monoisotopic.mass', mass.tol=0.01)
```

### Searching for a mass spectrum

All mass spectra databases (Mass CSV File and Mass SQLite) can be searched for mass spectra using the same function `searchMsEntries()`:
```r
myconn$searchMsEntries(mz.min=40, mz.max=41)
```
The function will return a character vector containing all identifiers of matching entries (i.e.: spectra containing at least one peak inside this M/Z range).

### Annotating a mass spectrum

Annotating a mass spectrum can be done either using a mass spectra database or a compound database.

When using a mass spectra database, the function to call is `searchMsPeaks()`:
```r
myMassConn$searchMsPeaks(myInputDataFrame, mz.tol=0.1, mz.tol.unit='plain', ms.mode='pos')
```
It returns a new data frame containing the annotations.

When using a compound database, the function to call is `annotateMzValues()`:
```r
myCompoundConn$annotateMzValues(myInputDataFrame, mz.tol=0.1, mz.tol.unit='plain', ms.mode='neg')
```
It returns a new data frame containing the annotations.

### Defining a new field

Defining a new field for a database is done in two steps, using definitions written inside a YAML file.

First we define the new field. Here we define the ChEBI database field for stars indicator (quality curation indicator):
```yaml
fields:
  n_stars:
    description: The ChEBI example stars indicator.
    class: integer
```

Then we define the parsing expression to use in ChEBI connector in order to parse the field's value:
```yaml
databases:
  chebi:
    parsing.expr:
      n_stars: //chebi:return/chebi:entityStar
```

We now have just to load the YAML file definition into biodb (in extension packages, this is done automatically):
```r
mybiodb$loadDefinitions('my_definitions.yml')
```

Parsing may be more complex for some fields or databases. In that case it is possible to write specific code in the database entry class for parsing these fields.

### Defining a new connector

Defining a new connector is done by writing two RC classes and a YAML definition:
 * An RC class for the connector, named `MyDatabaseConn.R`.
 * An RC class for the entry, named `MyDatabaseEntry.R`.
 * A definition YAML file containing metadata about the new connector, like:
  + The URLs (main URL, web service base URL, etc.) for a remote database.
  + The timing for querying a remote database (maximum number of requests per second).
  + The name.
  + The parsing expressions used for parsing the entry fields.
  + The type of content retrieved from the database when downloading an entry (plain text, XML, HTML, JSON, ...).

For a good starting example of defining a new remote connector, see *biodbChebi* the ChEBI extension for *biodb* at <https://github.com/pkrog/biodbChebi>. In particular:
 * [The connector class](https://github.com/pkrog/biodbChebi/blob/master/R/ChebiConn.R).
 * [The entry class](https://github.com/pkrog/biodbChebi/blob/master/R/ChebiEntry.R).
 * [The definitions file](https://github.com/pkrog/biodbChebi/blob/master/inst/definitions.yml).

## Documentation

Once in R, you can get an introduction to the package with:
```r
?biodb
```

Then each class has its own documentation. For instance, to get help about the `BiodbFactory` class:
```r
?biodb::BiodbFactory
```

Several vignettes are also available. To get a list of them run:
```r
vignette(package='biodb')
```

To open a vignette in a browser, use its name:
```r
vignette('new_connector', package='biodb')
```

## Contributing

If you wish to contribute to the *biodb* package, you first need to create an account under GitHub. You can then either ask to become a contributor or fork the project and submit a merge request.

Debugging, enhancement or creation of a database connector or an entry parser are of course most welcome.
