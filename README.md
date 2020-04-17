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


## Databases and fields accesible with biodb

*biodb* contains the following database connectors:

 * ChEBI.
 * Mass CSV File (an in-house database stored inside a CSV file).
 * Mass SQLite (an in-house database stored inside an SQLite file).

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
 * Peak table (containing M/Z, intensity, relative intensity, attribution, composition).
 * Chromatographic column name.
 * Chromatographic column length.
 * Chromatographic column diameter.
 * Chromatographic solvent.
 * Chromatographic retention time.
 * Chromatographic retention time unit.

## Installation

This development branch `dev_0.99` contains the latest developments. Its aim is the submission of the package to BioConductor.
You can install it with:
```r
devtools::install_github('pkrog/biodb', ref='dev_0.99', dependencies=TRUE, build_vignettes=FALSE)
```
In this version, connectors to web databases have been moved to separate repositories. You must thus install them separately. Here is an example for KEGG:
```r
devtools::install_github('pkrog/biodbKegg', dependencies=TRUE, build_vignettes=FALSE)
```

Full list of web database repositories available:
 * biodbHmdb.
 * biodbKegg.
 * biodbUniprot.

### Installation with bioconda

**biodb** is part of [bioconda](https://github.com/orgs/bioconda/dashboard), so you can install it using conda. This means also that it is possible to install it automatically in Galaxy, for a tool, if the conda system is enabled.

## Documentation

Once in R, you can get an introduction to the package with:
```r
?biodb
```

Then each class has its documentation. For instance, to get help about `ChebiConn` class:
```r
?biodb::ChebiConn
```

Some of the classes you can get help about are: `Biodb`, `BiodbFactory`, `BiodbConfig`, `BiodbPersistentCache`, `BiodbDbsInfo`, `BiodbEntryFields`, `BiodbObserver`, `BiodbConn`, `BiodbEntry`, `MassdbConn`, `RemotedbConn`, `ChebiConn`, `MassCsvFileConn`, `MassSqliteConn`.

Several vignettes are also available. To get a list of them run:
```r
vignette(package='biodb')
```

To open a vignette in a browser, use its name:
```r
vignette('init', package='biodb')
```

## Contributing

If you wish to contribute to the *biodb* package, you first need to create an account under GitHub. You can then either ask to become a contributor or fork the project and submit a merge request.

Debugging, enhancement or creation of a database connector or an entry parser are of course most welcome.

## Citations

### ChEBI

<http://www.ebi.ac.uk/chebi/>

 * Hastings, J., de Matos, P., Dekker, A., Ennis, M., Harsha, B., Kale, N., Muthukrishnan, V., Owen, G., Turner, S., Williams, M., and Steinbeck, C. (2013) The ChEBI reference database and ontology for biologically relevant chemistry: enhancements for 2013. Nucleic Acids Res, <http://dx.doi.org/10.1093/nar/gks1146>.
