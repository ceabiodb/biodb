Biodb
=====

[![Build Status](https://travis-ci.org/pkrog/biodb.svg?branch=master)](https://travis-ci.org/pkrog/biodb)

An R package for connecting to chemical and biological databases.

## Introduction

*biodb* is a framework for developing database connectors. It is delivered with connector examples (see next chapter).
For now, the targeted databases are the ones that store molecules, proteins, lipids and MS spectra. However other type of databases (NMR database for instance) could also be targeted.

With *biodb* you can:

 * Define your own database connector.
 * Access entries by accession number and let *biodb* download them for you.
 * Take advantage of the cache system, that saves the results of all sent requests for you. If you send again the same request, the cached result will be used instead of contacting the database. The cache system can be disabled.
 * Download locally a downloadable database and access entries by accession number locally.
 * Rely on *biodb* to access correctly the database, respecting the published access policy (i.e.: not sending too much requests). *biodb* uses a special class for scheduling requests on each database.
 * Switch from one database to another easily (providing they offer the same type of information), not changing a line in your code. This is because entries are populated with values found from the database, using always the same keys.
 * Search for MS and MSMS spectra by peaks in Mass spectra databases.
 * Export any database into a CSV file.

## Examples

### Getting entries from a remote database

Here is an example on how to retrieve entries from ChEBI database and get a data frames of them (you must first install both *biodb* and *biodbChebi* packages):
```r
bdb <- biodb::Biodb()
chebi <- bdb$getFactory()$createConn('chebi')
entries <- chebi$getEntry(c('2528', '7799', '15440'))
bdb$entriesToDataframe(entries)
```

## Installation

The package is currently in submission to BioConductor.

In the meantime you can install the latest stable version with:
```r
devtools::install_github('pkrog/biodb', dependencies=TRUE, build_vignettes=FALSE)
```

### Installation with bioconda

**biodb** is part of [bioconda](https://github.com/orgs/bioconda/dashboard), so you can install it using conda. This means also that it is possible to install it automatically in Galaxy, for a tool, if the conda system is enabled.

## Databases and fields accesible with biodb

The *biodb* package contains the following in-house database connectors:

 * Compound CSV File (an in-house database stored inside a CSV file).
 * Mass CSV File (an in-house database stored inside a CSV file).
 * Mass SQLite (an in-house database stored inside an SQLite file).

Alongside *biodb* you can install the following R extension packages that use  *biodb* for implementing connectors to online databases:

 * [biodbChebi](https://github.com/pkrog/biodbChebi) for accessing the [ChEBI](https://www.ebi.ac.uk/chebi/) database.
 * [biodbHmdb](https://github.com/pkrog/biodbHmdb) for accessing the [HMDB](http://www.hmdb.ca/) database.
 * [biodbKegg](https://github.com/pkrog/biodbKegg) for accessing the [KEGG](https://www.kegg.jp/) databases.
 * [biodbUniprot](https://github.com/pkrog/biodbUniprot) for accessing the [Uniprot](https://www.uniprot.org/) database.

Installation of one of those extension packages can be done with the following command (replace 'biodbKegg' with the name of the wanted package):
```r
devtools::install_github('pkrog/biodbKegg', dependencies=TRUE, build_vignettes=FALSE)
```

Here are some of the fields accessible through the retrieved entries:

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
