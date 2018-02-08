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

This software is licensed under the GNU Affero General Public License version 3 (AGPL-3.0).

## Installation

### Install from GitHub

Using package `devtools` you can install Biodb directly from GitHub:
```r
devtools::install_github('pkrog/biodb')
```

### Install from local repository

First, you need to clone the GitHub repos:
```bash
git clone -b master https://github.com/pkrog/biodb.git
```

Then you install *biodb* using the `devtools` package:
```r
devtools::install_local('/your/path/to/biodb')
```

## Examples

 * [Retrieving some entries from ChEBI database](examples/chebi-retrieve.R).
 * [Exporting some entries of miRBase Mature into a CSV file](examples/mirbase-tocsv.R).
 * [Search for MS spectra in Massbank Japan](examples/massbank.jp-ms-search.R).
 * [Search for MSMS spectra in Massbank Japan](examples/massbank.jp-msms-search.R).
 * [Integrating data from different databases](examples/integrating-different-dbs.R).
 * [Checking a table of database IDs](examples/checking-ids.R).
 * [Seach for compounds in KEGG Compound database](examples/kegg.compound-search.R).
 * [Extract spectra from Massbank to build a custom peak table](examples/massbank.jp-extract.R).

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

## Citations

| Database         | Home page                     | Publication
| ---------------- | ----------------------------- | ----------------------------------------------------------------
| ChEBI            | <http://www.ebi.ac.uk/chebi/> | Hastings, J., de Matos, P., Dekker, A., Ennis, M., Harsha, B., Kale, N., Muthukrishnan, V., Owen, G., Turner, S., Williams, M., and Steinbeck, C. (2013) The ChEBI reference database and ontology for biologically relevant chemistry: enhancements for 2013. Nucleic Acids Res. <http://dx.doi.org/10.1093/nar/gks1146>.
| ChemSpider       | <http://www.chemspider.com>   | Harry E. Pence and Antony Williams. ChemSpider: An Online Chemical Information Resource. Journal of Chemical Education 2010 87 (11), 1123-1124. <http://dx.doi.org/10.1021/ed100697w>.
| Expasy Enzyme    | <https://enzyme.expasy.org>   | Bairoch A. The ENZYME database in 2000. Nucleic Acids Res 28:304-305(2000). <https://enzyme.expasy.org/data/enz00.pdf>.
| ---------------- | ----------------------------- | ----------------------------------------------------------------
| HMDB             | <http://www.hmdb.ca>          | Wishart DS, Tzur D, Knox C, et al., HMDB: the Human Metabolome Database. Nucleic Acids Res. 2007 Jan;35(Database issue):D521-6. <https://doi.org/10.1093/nar/gkl923>.
|                  |                               | Wishart DS, Knox C, Guo AC, et al., HMDB: a knowledgebase for the human metabolome. Nucleic Acids Res. 2009 37(Database issue):D603-610. <https://doi.org/10.1093/nar/gkn810>.
|                  |                               | Wishart DS, Jewison T, Guo AC, Wilson M, Knox C, et al., HMDB 3.0 — The Human Metabolome Database in 2013. Nucleic Acids Res. 2013. Jan 1;41(D1):D801-7. <https://doi.org/10.1093/nar/gks1065>.
| ---------------- | ----------------------------- | ----------------------------------------------------------------
| KEGG Compound |
| Lipidmaps Structure |
| Massbank JP
| Mirbase Mature
| NCBI Gene
| NCBI CCDS
| NCBI Pubchem Comp
| NCBI Pubchem Subst
| Peakforest
| Uniprot
