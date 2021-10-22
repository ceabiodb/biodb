# Description of the `inst` folder content

## extdata folder

### Data files

TSV and SQLite files in `inst/extdata` folder have been constructed from
[ChEBI](https://www.ebi.ac.uk/chebi/), [ExPASy](https://www.expasy.org/),
[MassBank](https://massbank.eu/MassBank/) and
[UniProt](https://www.uniprot.org/), using
the *biodb* extensions [biodbChebi](https://github.com/pkrog/biodbChebi), 
[biodbExpasy](https://github.com/pkrog/biodbExpasy),
[biodbMassbank](https://github.com/pkrog/biodbMassbank),
and
[biodbUniprot](https://github.com/pkrog/biodbUniprot).

The files in `inst/extdata/generated` are conversion of the TSV files in `inst/extdata`, using *biodb*.

### Other files

The YAML and R files are example files for the vignettes.

## testref folder

Entry reference files for testing connectors.

## testsrc folder

Code files to be sourced during tests and shared by long tests and normal tests.
